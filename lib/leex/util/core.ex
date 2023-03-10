defmodule Leex.Util.Core do
  alias Leex.Util

  def generate_functions(rules, defs) do
    included_functions = get_included_functions()
    {regex_actions, actions} = get_actions(rules, defs)
    {dfa, dfa_first} = Util.DFA.make_dfa(regex_actions)

    action_functions = generate_action_functions(actions)
    dfa_functions = generate_dfa_functions(dfa, dfa_first)

    {included_functions, action_functions, dfa_functions}
  end

  defp get_included_functions do
    quote do
      def format_error({:illegal, error}) do
        "illegal characters #{error}"
      end

      def format_error({:user, error}), do: error

      def string(string), do: string(string, 1)

      def string(string, line), do: string(string, line, string, [])

      defp string("", line, "", tokens) do
        {:ok, yyrev(tokens), line}
      end

      defp string(string, line, token_string, tokens) do
        case yystate(yystate(), string, line, 0, :reject, 0) do
          # Accepting end state
          {accept, alen, string, line_1} ->
            string_cont(string, line_1, yyaction(accept, alen, token_string, line), tokens)

          # Accepting transition state
          {accept, alen, string, line_1, _s} ->
            string_cont(string, line_1, yyaction(accept, alen, token_string, line), tokens)

          # After a non-accepting state
          {:reject, _alen, tlen, _string, line_1, _s} ->
            {:error, {:line, __MODULE__, {:illegal, yypre(token_string, tlen + 1)}}, line_1,
             tokens}

          {accept, alen, tlen, _string, line_1, _s} ->
            token_string_2 = yysuf(token_string, alen)
            line_2 = adjust_line(tlen, alen, token_string_2, line_1)

            string_cont(
              token_string_2,
              line_2,
              yyaction(accept, alen, token_string, line),
              tokens
            )
        end
      end

      defp string_cont(string, line, {:token, token}, tokens) do
        string(string, line, string, [token | tokens])
      end

      defp string_cont(string, line, {:token, token, push}, tokens) do
        string = push <> string
        string(string, line, string, [token | tokens])
      end

      defp string_cont(string, line, {:end_token, token}, tokens) do
        string(string, line, string, [token | tokens])
      end

      defp string_cont(string, line, {:end_token, token, push}, tokens) do
        string = push <> string
        string(string, line, string, [token | tokens])
      end

      defp string_cont(string, line, :skip_token, tokens) do
        string(string, line, string, tokens)
      end

      defp string_cont(string, line, {:skip_token, push}, tokens) do
        string = push <> string
        string(string, line, string, tokens)
      end

      defp string_cont(_string, line, {:error, error}, _tokens) do
        {:error, {line, __MODULE__, {:user, error}}, line}
      end

      def token(continuation, string), do: token(continuation, string, 1)

      def token(string, [], line) do
        token(yystate(), string, line, string, 0, line, :reject, 0)
      end

      def token(
            {:token, state, line, token_string, token_len, token_line, accept_action, accept_len},
            string,
            _
          ) do
        token(
          state,
          string,
          line,
          token_string <> string,
          token_len,
          token_line,
          accept_action,
          accept_len
        )
      end

      defp token(
             state,
             string,
             line,
             token_string,
             token_len,
             token_line,
             accept_action,
             accept_len
           ) do
        case yystate(state, string, line, token_len, accept_action, accept_len) do
          # Accepting end state, we have a token.
          {accept_action, accept_len, string, line} ->
            token_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_string, token_line)
            )

          # Accepting transition state, can take more chars.
          # Need more chars to check
          {accept_action, accept_len, [], line, state} ->
            {:more,
             {:token, state, line, token_string, accept_len, token_line, accept_action,
              accept_len}}

          # Take what we got
          {accept_action, accept_len, string, line, _state} ->
            token_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_string, token_line)
            )

          # After a non-accepting state, maybe reach accept state later.
          # Need more chars to check
          {accept_action, accept_len, token_len, [], line, state} ->
            {:more,
             {:token, state, line, token_string, token_len, token_line, accept_action, accept_len}}

          # No token match
          {:reject, _accept_len, token_len, :eof, line, _state} ->
            # Check for partial token which is error.
            return_value =
              if token_len > 0 do
                {:error,
                 {
                   token_line,
                   __MODULE__,
                   # Skip eof tail in token_val.
                   {:illegal, yypre(token_string, token_len)}
                 }, line}
              else
                {:eof, line}
              end

            {:done, return_value, :eof}

          # No token match
          {:reject, _accept_len, token_len, string, line, _state} ->
            error = {token_line, __MODULE__, {:illegal, yypre(token_string, token_len + 1)}}
            {:done, {:error, error, line}, string}

          # Use last accept match
          {accept_action, accept_len, token_len, _string, line, _state} ->
            token_string_2 = yysuf(token_string, accept_len)
            line = adjust_line(token_len, accept_len, token_string_2, line)

            token_cont(
              token_string_2,
              line,
              yyaction(accept_action, accept_len, token_string, token_line)
            )
        end
      end

      defp token_cont(string, line, {:token, token}) do
        {:done, {:ok, token, line}, string}
      end

      defp token_cont(string, line, {:token, token, push}) do
        string = push <> string
        {:done, {:ok, token, line}, string}
      end

      defp token_cont(string, line, {:end_token, token}) do
        {:done, {:ok, token, line}, string}
      end

      defp token_cont(string, line, {:end_token, token, push}) do
        string = push <> string
        {:done, {:ok, token, line}, string}
      end

      defp token_cont(string, line, :skip_token) do
        token(yystate(), string, line, string, 0, line, :reject, 0)
      end

      defp token_cont(string, line, {:skip_token, push}) do
        string = push <> string
        token(yystate(), string, line, string, 0, line, :reject, 0)
      end

      defp token_cont(string, line, {:error, error}) do
        {:done, {:error, {line, __MODULE__, {:user, error}}, line}, string}
      end

      def tokens(continuation, string), do: tokens(continuation, string, 1)

      def tokens([], string, line) do
        tokens(yystate(), string, line, string, 0, line, [], :reject, 0)
      end

      def tokens(
            {:tokens, state, line, token_val, token_len, token_line, tokens, accept_action,
             accept_len},
            string,
            _
          ) do
        tokens(
          state,
          string,
          line,
          token_val <> string,
          token_len,
          token_line,
          tokens,
          accept_action,
          accept_len
        )
      end

      def tokens(
            {:skip_tokens, state, line, token_val, token_len, token_line, error, accept_action,
             accept_len},
            string,
            _
          ) do
        skip_tokens(
          state,
          string,
          line,
          token_val <> string,
          token_len,
          token_line,
          error,
          accept_action,
          accept_len
        )
      end

      defp tokens(
             state,
             string,
             line,
             token_val,
             token_len,
             token_line,
             tokens,
             accept_action,
             accept_len
           ) do
        case yystate(state, string, line, token_len, accept_action, accept_len) do
          # Accepting end state, we have a token.
          {accept_action, accept_len, string, line} ->
            tokens_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_val, token_line),
              tokens
            )

          # Accepting transition state, can take more chars.
          # Need more chars to check
          {accept_action, accept_len, [], line, state} ->
            {:more,
             {:tokens, state, line, token_val, accept_len, token_line, tokens, accept_action,
              accept_len}}

          # Take what we got
          {accept_action, accept_len, string, line, _state} ->
            tokens_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_val, token_line),
              tokens
            )

          # After a non-accepting state, maybe reach accept state later.
          # Need more chars to check
          {accept_action, accept_len, token_len, [], line, state} ->
            {:more,
             {:tokens, state, line, token_val, token_len, token_line, tokens, accept_action,
              accept_len}}

          # No token match
          {:reject, _accept_len, token_len, :eof, line, _state} ->
            # Check for partial token which is error, no need to skip here.
            return_val =
              cond do
                token_len > 0 ->
                  {:error,
                   {
                     token_line,
                     __MODULE__,
                     # Skip eof tail in token_val.
                     {:illegal, yypre(token_val, token_len)}
                   }, line}

                tokens == [] ->
                  {:eof, line}

                true ->
                  {:ok, yyrev(tokens), line}
              end

            {:done, return_val, :eof}

          {:reject, _accept_len, token_len, _string, line, _state} ->
            # Skip rest of tokens.
            error = {line, __MODULE__, {:illegal, yypre(token_val, token_len + 1)}}
            skip_tokens(yysuf(token_val, token_len + 1), line, error)

          {accept_action, accept_len, token_len, _string, line, _state} ->
            token = yyaction(accept_action, accept_len, token_val, token_line)
            token_val = yysuf(token_val, accept_len)
            line = adjust_line(token_len, accept_len, token_val, line)
            tokens_cont(token_val, line, token, tokens)
        end
      end

      defp tokens_cont(string, line, {:token, token}, tokens) do
        tokens(yystate(), string, line, string, 0, line, [token | tokens], :reject, 0)
      end

      defp tokens_cont(string, line, {:token, token, push}, tokens) do
        string = push <> string
        tokens(yystate(), string, line, string, 0, line, [token | tokens], :reject, 0)
      end

      defp tokens_cont(string, line, {:end_token, token}, tokens) do
        {:done, {:ok, yyrev(tokens, [token]), line}, string}
      end

      defp tokens_cont(string, line, {:end_token, token, push}, tokens) do
        string = push <> string
        {:done, {:ok, yyrev(tokens, [token]), line}, string}
      end

      defp tokens_cont(string, line, :skip_token, tokens) do
        tokens(yystate(), string, line, string, 0, line, tokens, :reject, 0)
      end

      defp tokens_cont(string, line, {:skip_token, push}, tokens) do
        string = push <> string
        tokens(yystate(), string, line, string, 0, line, tokens, :reject, 0)
      end

      defp tokens_cont(string, line, {:error, error}, _tokens) do
        skip_tokens(string, line, {line, __MODULE__, {:user, error}})
      end

      defp skip_tokens(string, line, error) do
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_tokens(
             state,
             string,
             line,
             token_val,
             token_len,
             token_line,
             error,
             accept_action,
             accept_len
           ) do
        case yystate(state, string, line, token_len, accept_action, accept_len) do
          # Accepting end state
          {accept_action, accept_len, string, line} ->
            skip_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_val, token_line),
              error
            )

          # After an accepting state
          {accept_action, accept_len, [], line, string} ->
            {:more,
             {:skip_tokens, string, line, token_val, accept_len, token_line, error, accept_action,
              accept_len}}

          {accept_action, accept_len, string, line, _s} ->
            skip_cont(
              string,
              line,
              yyaction(accept_action, accept_len, token_val, token_line),
              error
            )

          # After a non-accepting state
          {accept_action, accept_len, token_len, [], line, string} ->
            {:more,
             {:skip_tokens, string, line, token_val, token_len, token_line, error, accept_action,
              accept_len}}

          {:reject, _accept_len, _token_len, :eof, line, _s} ->
            {:done, {:error, error, line}, :eof}

          {:reject, _accept_len, token_len, _string, line, _s} ->
            skip_tokens(yysuf(token_val, token_len + 1), line, error)

          {accept_action, accept_len, token_len, _string, line, _s} ->
            token = yyaction(accept_action, accept_len, token_val, token_line)
            token_val = yysuf(token_val, accept_len)
            line = adjust_line(token_len, accept_len, token_val, line)
            skip_cont(token_val, line, token, error)
        end
      end

      defp skip_cont(string, line, {:token, _token}, error) do
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:token, _token, push}, error) do
        string = push <> string
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:end_token, _token}, error) do
        {:done, {:error, error, line}, string}
      end

      defp skip_cont(string, line, {:end_token, _token, push}, error) do
        string = push <> string
        {:done, {:error, error, line}, string}
      end

      defp skip_cont(string, line, :skip_token, error) do
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:skip_token, push}, error) do
        string = push <> string
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:error, _error}, error) do
        skip_tokens(yystate(), string, line, string, 0, line, error, :reject, 0)
      end

      defp yyrev(list), do: Enum.reverse(list)
      defp yyrev(list, tail), do: Enum.reverse(list, tail)
      defp yypre(string, n), do: String.slice(string, 0..(n - 1))
      defp yysuf(string, n), do: Enum.drop(string, n)

      defp adjust_line(token_len, token_len, _string, line), do: line

      defp adjust_line(token_len, accept_len, <<?\n, string::binary>>, line) do
        adjust_line(token_len - 1, accept_len, string, line - 1)
      end

      defp adjust_line(token_len, accept_len, <<_, string::binary>>, line) do
        adjust_line(token_len - 1, accept_len, string, line)
      end
    end
  end

  defp generate_action_functions(actions) do
    actions = prep_actions(actions)

    (Enum.flat_map(actions, fn {action, _code, [_, _, line, tcs, len], name, _args, args_string} ->
       [
         "defp yyaction(#{action}, #{len},#{tcs}, #{line}) do",
         if tcs != "_" do
           "token_val = yypre(yy_tcs, token_len)"
         end,
         "#{name}(#{args_string})",
         "end"
       ]
       |> Enum.filter(&(&1 != nil))
     end) ++
       ["defp yyaction(_, _, _, _), do: :error"] ++
       Enum.flat_map(actions, fn {_action, code, _vars, name, _args, args_string} ->
         [
           "defp #{name}(#{args_string}) do",
           Macro.to_string(code),
           "end"
         ]
       end))
    |> Enum.join("\n")
    |> Code.string_to_quoted!()
  end

  defp generate_dfa_functions(dfa, dfa_first) do
    (["defp yystate(), do: #{dfa_first}"] ++
       Enum.flat_map(dfa, &get_state_code/1) ++
       ["defp yystate(s, ics, line, tlen, action, alen), do: {action,alen,tlen,ics,line,s}"])
    |> Enum.join("\n")
    |> Code.string_to_quoted!()
  end

  defp get_actions(rules, defs) do
    Enum.with_index(rules)
    |> Enum.reduce({[], []}, fn {{rule, context}, index}, {regex_actions, actions} ->
      case Util.Regex.parse_rule_regexp(rule, defs) do
        {:ok, regex} ->
          [token_val, token_len, token_line] =
            Enum.map(["token_val", "token_len", "token_line"], &var_used(context, &1))

          # Check for token variables.
          {regex_actions ++ [{regex, index}],
           actions ++ [{index, context, token_val, token_len, token_line}]}

        {:error, error} ->
          throw(error)
      end
    end)
  end

  defp prep_actions(actions) do
    actions
    |> Enum.map(fn
      {action, :empty_action} ->
        {action, :empty_action}

      {action, code, token_val, token_len, token_line} ->
        vars =
          [
            {token_val, "token_val"},
            {token_len, "token_len"},
            {token_line, "token_line"},
            {token_val, "yy_tcs"},
            {token_len or token_val, "token_len"}
          ]
          |> Enum.map(fn {f, s} ->
            if f do
              s
            else
              "_"
            end
          end)

        name = String.to_atom("yy_action_#{action}")
        [val, len, line, _, _] = vars
        args = for v <- [val, len, line], v != "_", do: v
        args_string = Enum.join(args, ", ")
        {action, code, vars, name, args, args_string}
    end)
  end

  defp var_used(context, name) do
    code = Macro.to_string(context)
    Regex.match?(~r/(?<![\w:])#{name}(?![\w(])/, code)
  end

  defp get_state_code(%Leex.DfaState{no: no, trans: [], accept: {:accept, accept}}) do
    [
      "defp yystate(#{no}, ics, line, tlen, _, _) do",
      "{#{accept},tlen,ics,line}",
      "end"
    ]
  end

  defp get_state_code(%Leex.DfaState{no: no, trans: trans, accept: {:accept, accept}}) do
    (trans |> pack_trans() |> Enum.flat_map(&get_accept_state_code(no, accept, &1))) ++
      [
        "defp yystate(#{no}, ics, line, tlen, _, _) do",
        "{#{accept},tlen,ics,line,#{no}}",
        "end"
      ]
  end

  defp get_state_code(%Leex.DfaState{no: no, trans: trans, accept: :noaccept}) do
    (trans |> pack_trans() |> Enum.flat_map(&get_noaccept_state_code(no, &1))) ++
      [
        "defp yystate(#{no}, ics, line, tlen, action, alen) do",
        "{action,alen,tlen,ics,line,#{no}}",
        "end"
      ]
  end

  # region Accept

  defp get_accept_state_code(next_state, action, {{char, :maxchar}, state}) do
    get_accept_head_max(next_state, char) ++
      get_accept_body(state, "line", action)
  end

  defp get_accept_state_code(next_state, action, {{char_1, char_2}, state}) do
    get_accept_head_range(next_state, char_1, char_2) ++
      get_accept_body(state, "line", action)
  end

  defp get_accept_state_code(next_state, action, {?\n, state}) do
    get_accept_head_1(next_state, ?\n) ++
      get_accept_body(state, "line+1", action)
  end

  defp get_accept_state_code(next_state, action, {char, state}) do
    get_accept_head_1(next_state, char) ++
      get_accept_body(state, "line", action)
  end

  defp get_accept_head_max(state, min) do
    get_head_max(state, min, "_", "_")
  end

  defp get_accept_head_range(state, min, max) do
    get_head_range(state, min, max, "_", "_")
  end

  defp get_accept_head_1(state, char) do
    get_head_1(state, char, "_", "_")
  end

  defp get_accept_body(next, line, action) do
    get_body(next, line, :io_lib.write(action), "tlen")
  end

  # endregion

  # region No Accept

  defp get_noaccept_state_code(next_state, {{char, :maxchar}, state}) do
    get_noaccept_head_max(next_state, char) ++
      get_noaccept_body(state, "line")
  end

  defp get_noaccept_state_code(next_state, {{char_1, char_2}, state}) do
    get_noaccept_head_range(next_state, char_1, char_2) ++
      get_noaccept_body(state, "line")
  end

  defp get_noaccept_state_code(next_state, {?\n, state}) do
    get_noaccept_head_1(next_state, ?\n) ++
      get_noaccept_body(state, "line+1")
  end

  defp get_noaccept_state_code(next_state, {char, state}) do
    get_noaccept_head_1(next_state, char) ++
      get_noaccept_body(state, "line")
  end

  defp get_noaccept_head_max(state, min) do
    get_head_max(state, min, "action", "alen")
  end

  defp get_noaccept_head_range(state, min, max) do
    get_head_range(state, min, max, "action", "alen")
  end

  defp get_noaccept_head_1(state, char) do
    get_head_1(state, char, "action", "alen")
  end

  defp get_noaccept_body(next, line) do
    get_body(next, line, "action", "alen")
  end

  # endregion

  defp get_head_max(state, min, action, alen) do
    [
      "defp yystate(#{state}, <<c,ics::binary>>, line, tlen, #{action}, #{alen}) when c >= #{min} do"
    ]
  end

  defp get_head_range(state, min, max, action, alen) do
    [
      "defp yystate(#{state}, <<c,ics::binary>>, line, tlen, #{action}, #{alen}) when c >= #{min} and c <= #{max} do"
    ]
  end

  defp get_head_1(state, char, action, alen) do
    [
      "defp yystate(#{state}, <<#{char},ics::binary>>, line, tlen, #{action}, #{alen}) do"
    ]
  end

  defp get_body(next, line, action, alen) do
    [
      "yystate(#{next}, ics, #{line}, tlen+1, #{action}, #{alen})",
      "end"
    ]
  end

  defp pack_trans(trans), do: pack_trans(trans, [])

  defp pack_trans([{{char, char}, state} | trans], packed) do
    if Enum.member?(packed, {char, state}) do
      pack_trans(trans, packed)
    else
      pack_trans(trans, [{char, state} | packed])
    end
  end

  defp pack_trans([{{char, ?\n}, state} | trans], packed) do
    pack_trans([{{char, ?\n - 1}, state} | trans], [{?\n, state} | packed])
  end

  defp pack_trans([{{?\n, char}, state} | trans], packed) do
    pack_trans([{{?\n + 1, char}, state} | trans], [{?\n, state} | packed])
  end

  defp pack_trans([{{char_1, char_2}, state} | trans], packed)
       when char_1 < ?\n and char_2 > ?\n do
    pack_trans([{{char_1, ?\n - 1}, state}, {{?\n + 1, char_2}, state} | trans], [
      {?\n, state} | packed
    ])
  end

  defp pack_trans([{{char_1, char_2}, state} | trans], packed) when char_2 == char_1 + 1 do
    pack_trans(trans, [{char_1, state}, {char_2, state} | packed])
  end

  defp pack_trans([trans | rest_trans], packed) do
    pack_trans(rest_trans, packed ++ [trans])
  end

  defp pack_trans([], packed), do: packed
end

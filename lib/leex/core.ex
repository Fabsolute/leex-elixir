defmodule Leex.Core do
  alias Leex.Util
  alias Leex.{DFA, Included}

  def generate_functions(rules, defs) do
    {regex_actions, actions} = get_actions(rules, defs)
    {dfa, dfa_first} = DFA.make_dfa(regex_actions)

    included_functions = Included.get_included_functions(dfa_first)
    action_functions = generate_action_functions(actions)
    dfa_functions = generate_dfa_functions(dfa)

    quote do
      unquote(included_functions)
      unquote(action_functions)
      unquote(dfa_functions)
    end
  end

  defp generate_action_functions(actions) do
    actions = prep_actions(actions)

    for {action, code, [token_line, yy_tcs, token_len]} <- actions do
      token_val_definition =
        if yy_tcs != :_ do
          quote do
            unquote(Macro.var(:token_val, nil)) =
              Runtime.Helper.prefix(unquote(Macro.var(yy_tcs, nil)), unquote(Macro.var(token_len, nil)))
          end
        end

      quote do
        defp leex_action(
               unquote(action),
               unquote(Macro.var(token_len, nil)),
               unquote(Macro.var(yy_tcs, nil)),
               unquote(Macro.var(token_line, nil))
             ) do
          unquote(token_val_definition)
          unquote(code)
        end
      end
    end
  end

  defp generate_dfa_functions(dfa) do
    states = dfa |> Enum.map(&get_state_code/1)

    quote do
      unquote(states)
      defp leex_state(s, ics, line, tlen, action, alen), do: {action, alen, tlen, ics, line, s}
    end
  end

  defp get_actions(rules, defs) do
    Enum.with_index(rules)
    |> Enum.reduce({[], []}, fn {{rule, context}, index}, {regex_actions, actions} ->
      case Util.Regex.parse_rule_regexp(rule, defs) do
        {:ok, regex} ->
          [token_val, token_len, token_line] =
            Enum.map(
              [:token_val, :token_len, :token_line],
              &Util.AST.var_used(context, &1)
            )

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
            {token_line, :token_line},
            {token_val, :yy_tcs},
            {token_len or token_val, :token_len}
          ]
          |> Enum.map(fn {f, s} ->
            if f do
              s
            else
              :_
            end
          end)

        {action, code, vars}
    end)
  end

  defp get_state_code(%DFA{no: no, trans: [], accept: {:accept, accept}}) do
    quote do
      defp leex_state(unquote(no), ics, line, tlen, _, _) do
        {unquote(accept), tlen, ics, line}
      end
    end
  end

  defp get_state_code(%DFA{no: no, trans: trans, accept: {:accept, accept}}) do
    accept_state_ast = trans |> pack_trans |> Enum.map(&get_accept_state_code(no, accept, &1))

    quote do
      unquote(accept_state_ast)

      defp leex_state(unquote(no), ics, line, tlen, _, _) do
        {unquote(accept), tlen, ics, line, unquote(no)}
      end
    end
  end

  defp get_state_code(%DFA{no: no, trans: trans, accept: :noaccept}) do
    noaccept_state_ast = trans |> pack_trans |> Enum.map(&get_noaccept_state_code(no, &1))

    quote do
      unquote(noaccept_state_ast)

      defp leex_state(unquote(no), ics, line, tlen, action, alen) do
        {action, alen, tlen, ics, line, unquote(no)}
      end
    end
  end

  # region Accept

  defp get_accept_state_code(next_state, action, {{char, :maxchar}, state}) do
    get_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> get_accept_max_code(next_state, char)
  end

  defp get_accept_state_code(next_state, action, {{char_1, char_2}, state}) do
    get_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> get_accept_range_code(next_state, char_1, char_2)
  end

  defp get_accept_state_code(next_state, action, {?\n, state}) do
    get_accept_body(
      state,
      quote do
        line + 1
      end,
      action
    )
    |> get_accept_1_code(next_state, ?\n)
  end

  defp get_accept_state_code(next_state, action, {char, state}) do
    get_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> get_accept_1_code(next_state, char)
  end

  defp get_accept_max_code(body, state, min) do
    get_max_code(body, state, min, Macro.var(:_, __MODULE__), Macro.var(:_, __MODULE__))
  end

  defp get_accept_range_code(body, state, min, max) do
    get_range_code(
      body,
      state,
      min,
      max,
      Macro.var(:_, __MODULE__),
      Macro.var(:_, __MODULE__)
    )
  end

  defp get_accept_1_code(body, state, char) do
    get_1_code(body, state, char, Macro.var(:_, __MODULE__), Macro.var(:_, __MODULE__))
  end

  defp get_accept_body(next, line, action) do
    get_body(next, line, action, Macro.var(:tlen, __MODULE__))
  end

  # endregion

  # region No Accept

  defp get_noaccept_state_code(next_state, {{char, :maxchar}, state}) do
    get_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> get_noaccept_max_code(next_state, char)
  end

  defp get_noaccept_state_code(next_state, {{char_1, char_2}, state}) do
    get_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> get_noaccept_range_code(next_state, char_1, char_2)
  end

  defp get_noaccept_state_code(next_state, {?\n, state}) do
    get_noaccept_body(
      state,
      quote do
        line + 1
      end
    )
    |> get_noaccept_1_code(next_state, ?\n)
  end

  defp get_noaccept_state_code(next_state, {char, state}) do
    get_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> get_noaccept_1_code(next_state, char)
  end

  defp get_noaccept_max_code(body, state, min) do
    get_max_code(body, state, min, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  defp get_noaccept_range_code(body, state, min, max) do
    get_range_code(
      body,
      state,
      min,
      max,
      Macro.var(:action, __MODULE__),
      Macro.var(:alen, __MODULE__)
    )
  end

  defp get_noaccept_1_code(body, state, char) do
    get_1_code(body, state, char, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  defp get_noaccept_body(next, line) do
    get_body(next, line, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  # endregion

  defp get_max_code(body, state, min, action, alen) do
    quote do
      defp leex_state(unquote(state), <<c, ics::binary>>, line, tlen, unquote(action), unquote(alen))
           when c >= unquote(min) do
        unquote(body)
      end
    end
  end

  defp get_range_code(body, state, min, max, action, alen) do
    quote do
      defp leex_state(
             unquote(state),
             <<c, ics::binary>>,
             line,
             tlen,
             unquote(action),
             unquote(alen)
           )
           when c >= unquote(min) and c <= unquote(max) do
        unquote(body)
      end
    end
  end

  defp get_1_code(body, state, char, action, alen) do
    quote do
      defp leex_state(
             unquote(state),
             <<unquote(char), ics::binary>>,
             line,
             tlen,
             unquote(action),
             unquote(alen)
           ) do
        unquote(body)
      end
    end
  end

  defp get_body(next, line, action, alen) do
    quote do
      leex_state(
        unquote(next),
        ics,
        unquote(line),
        tlen + 1,
        unquote(action),
        unquote(alen)
      )
    end
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

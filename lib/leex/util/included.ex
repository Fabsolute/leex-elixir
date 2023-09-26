defmodule Leex.Util.Included do
  def get_included_functions(initial_state) do
    quote do
      @initial_state unquote(initial_state)
      def test() do
        string("[1,2,3,4,5,6, :ahmet, [:turk, 4]]")
      end

      def string(string), do: string(string, 1)

      def string(string, line), do: string(string, line, string, [])

      defp string("", line, "", tokens) do
        {:ok, yyrev(tokens), line}
      end

      defp string(string, line, token_string, tokens) do
        case yystate(@initial_state, string, line, 0, :reject, 0) do
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
        token(@initial_state, string, line, string, 0, line, :reject, 0)
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
        token(@initial_state, string, line, string, 0, line, :reject, 0)
      end

      defp token_cont(string, line, {:skip_token, push}) do
        string = push <> string
        token(@initial_state, string, line, string, 0, line, :reject, 0)
      end

      defp token_cont(string, line, {:error, error}) do
        {:done, {:error, {line, __MODULE__, {:user, error}}, line}, string}
      end

      def tokens(continuation, string), do: tokens(continuation, string, 1)

      def tokens([], string, line) do
        tokens(@initial_state, string, line, string, 0, line, [], :reject, 0)
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
        tokens(@initial_state, string, line, string, 0, line, [token | tokens], :reject, 0)
      end

      defp tokens_cont(string, line, {:token, token, push}, tokens) do
        string = push <> string
        tokens(@initial_state, string, line, string, 0, line, [token | tokens], :reject, 0)
      end

      defp tokens_cont(string, line, {:end_token, token}, tokens) do
        {:done, {:ok, yyrev(tokens, [token]), line}, string}
      end

      defp tokens_cont(string, line, {:end_token, token, push}, tokens) do
        string = push <> string
        {:done, {:ok, yyrev(tokens, [token]), line}, string}
      end

      defp tokens_cont(string, line, :skip_token, tokens) do
        tokens(@initial_state, string, line, string, 0, line, tokens, :reject, 0)
      end

      defp tokens_cont(string, line, {:skip_token, push}, tokens) do
        string = push <> string
        tokens(@initial_state, string, line, string, 0, line, tokens, :reject, 0)
      end

      defp tokens_cont(string, line, {:error, error}, _tokens) do
        skip_tokens(string, line, {line, __MODULE__, {:user, error}})
      end

      defp skip_tokens(string, line, error) do
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
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
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:token, _token, push}, error) do
        string = push <> string
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:end_token, _token}, error) do
        {:done, {:error, error, line}, string}
      end

      defp skip_cont(string, line, {:end_token, _token, push}, error) do
        string = push <> string
        {:done, {:error, error, line}, string}
      end

      defp skip_cont(string, line, :skip_token, error) do
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:skip_token, push}, error) do
        string = push <> string
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
      end

      defp skip_cont(string, line, {:error, _error}, error) do
        skip_tokens(@initial_state, string, line, string, 0, line, error, :reject, 0)
      end

      defp yyrev(list), do: Enum.reverse(list)
      defp yyrev(list, tail), do: Enum.reverse(list, tail)
      defp yypre(string, n), do: String.slice(string, 0..(n - 1))
      defp yysuf(string, n), do: String.slice(string, n..-1)

      defp adjust_line(token_len, token_len, _string, line), do: line

      defp adjust_line(token_len, accept_len, <<?\n, string::binary>>, line) do
        adjust_line(token_len - 1, accept_len, string, line - 1)
      end

      defp adjust_line(token_len, accept_len, <<_, string::binary>>, line) do
        adjust_line(token_len - 1, accept_len, string, line)
      end
    end
  end
end

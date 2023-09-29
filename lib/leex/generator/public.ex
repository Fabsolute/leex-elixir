defmodule Leex.Generator.Public do
  def generate_public(initial_state) do
    quote do
      alias Leex.Runtime

      def test() do
        {:ok,
         [
           {:"[", 1},
           {:int, 1, 1},
           {:",", 1},
           {:int, 1, 2},
           {:",", 1},
           {:int, 1, 3},
           {:",", 1},
           {:int, 1, 4},
           {:",", 1},
           {:int, 1, 5},
           {:",", 1},
           {:int, 1, 6},
           {:",", 1},
           {:atom, 1, :ahmet},
           {:",", 1},
           {:"[", 1},
           {:atom, 1, :turk},
           {:",", 1},
           {:int, 1, 4},
           {:"]", 1},
           {:"]", 1}
         ],
         1} =
          string("[1,2,3,4,5,6, :ahmet, [:turk, 4]]")
      end

      def string(string), do: string(string, 1)

      def string(string, line) do
        Runtime.String.string(
          get_config(),
          string,
          line,
          string,
          []
        )
      end

      def token(continuation, string), do: token(continuation, string, 1)

      def token(string, [], line) do
        Runtime.Token.token(
          get_config(),
          unquote(initial_state),
          string,
          line,
          string,
          0,
          line,
          :reject,
          0
        )
      end

      def token(
            {:token, state, line, token_string, token_len, token_line, accept_action, accept_len},
            string,
            _
          ) do
        Runtime.Token.token(
          get_config(),
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

      def tokens(continuation, string), do: tokens(continuation, string, 1)

      def tokens([], string, line) do
        Runtime.Tokens.tokens(
          get_config(),
          unquote(initial_state),
          string,
          line,
          string,
          0,
          line,
          [],
          :reject,
          0
        )
      end

      def tokens(
            {:tokens, state, line, token_val, token_len, token_line, tokens, accept_action,
             accept_len},
            string,
            _
          ) do
        Runtime.Tokens.tokens(
          get_config(),
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
        Runtime.SkipTokens.skip_tokens(
          get_config(),
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

      @compile {:inline, get_config: 0}
      defp get_config() do
        %{
          initial_state: unquote(initial_state),
          module: __MODULE__,
          state: &leex_state/6,
          action: &leex_action/4
        }
      end
    end
  end
end

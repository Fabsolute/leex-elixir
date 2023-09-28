defmodule Leex.Included do
  def get_included_functions(initial_state) do
    quote do
      alias Leex.Runtime

      @initial_state unquote(initial_state)
      def test() do
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
          @initial_state,
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
          @initial_state,
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
          initial_state: @initial_state,
          module: __MODULE__,
          state: &yystate/6,
          action: &yyaction/4
        }
      end
    end
  end
end

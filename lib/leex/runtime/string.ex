defmodule Leex.Runtime.String do
  alias Leex.Runtime.Helper

  def string(_config, "", line, "", tokens) do
    {:ok, Helper.reverse(tokens), line}
  end

  def string(config, string, line, token_string, tokens) do
    case config.state.(config.initial_state, string, line, 0, :reject, 0) do
      # Accepting end state
      {accept, alen, string, line_1} ->
        string_cont(
          config,
          string,
          line_1,
          config.action.(accept, alen, token_string, line),
          tokens
        )

      # Accepting transition state
      {accept, alen, string, line_1, _s} ->
        string_cont(
          config,
          string,
          line_1,
          config.action.(accept, alen, token_string, line),
          tokens
        )

      # After a non-accepting state
      {:reject, _alen, tlen, _string, line_1, _s} ->
        {:error, {:line, config.module, {:illegal, Helper.prefix(token_string, tlen + 1)}}, line_1,
         tokens}

      {accept, alen, tlen, _string, line_1, _s} ->
        token_string_2 = Helper.suffix(token_string, alen)
        line_2 = Helper.adjust_line(tlen, alen, token_string_2, line_1)

        string_cont(
          config,
          token_string_2,
          line_2,
          config.action.(accept, alen, token_string, line),
          tokens
        )
    end
  end

  defp string_cont(config, string, line, {:token, token}, tokens) do
    string(config, string, line, string, [token | tokens])
  end

  defp string_cont(config, string, line, {:token, token, push}, tokens) do
    string = push <> string
    string(config, string, line, string, [token | tokens])
  end

  defp string_cont(config, string, line, {:end_token, token}, tokens) do
    string(config, string, line, string, [token | tokens])
  end

  defp string_cont(config, string, line, {:end_token, token, push}, tokens) do
    string = push <> string
    string(config, string, line, string, [token | tokens])
  end

  defp string_cont(config, string, line, :skip_token, tokens) do
    string(config, string, line, string, tokens)
  end

  defp string_cont(config, string, line, {:skip_token, push}, tokens) do
    string = push <> string
    string(config, string, line, string, tokens)
  end

  defp string_cont(config, _string, line, {:error, error}, _tokens) do
    {:error, {line, config.module, {:user, error}}, line}
  end
end

defmodule Leex.Runtime.SkipTokens do
  alias Leex.Runtime.Helper

  def skip_tokens(config, string, line, error) do
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end

  def skip_tokens(
         config,
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
    case config.state.(state, string, line, token_len, accept_action, accept_len) do
      # Accepting end state
      {accept_action, accept_len, string, line} ->
        skip_cont(
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_val, token_line),
          error
        )

      # After an accepting state
      {accept_action, accept_len, [], line, string} ->
        {:more,
          {:skip_tokens, string, line, token_val, accept_len, token_line, error, accept_action,
            accept_len}}

      {accept_action, accept_len, string, line, _s} ->
        skip_cont(
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_val, token_line),
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
        skip_tokens(config, Helper.suffix(token_val, token_len + 1), line, error)

      {accept_action, accept_len, token_len, _string, line, _s} ->
        token = config.action.(accept_action, accept_len, token_val, token_line)
        token_val = Helper.suffix(token_val, accept_len)
        line = Helper.adjust_line(token_len, accept_len, token_val, line)
        skip_cont(config, token_val, line, token, error)
    end
  end

  defp skip_cont(config, string, line, {:token, _token}, error) do
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end

  defp skip_cont(config, string, line, {:token, _token, push}, error) do
    string = push <> string
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end

  defp skip_cont(_config, string, line, {:end_token, _token}, error) do
    {:done, {:error, error, line}, string}
  end

  defp skip_cont(_config, string, line, {:end_token, _token, push}, error) do
    string = push <> string
    {:done, {:error, error, line}, string}
  end

  defp skip_cont(config, string, line, :skip_token, error) do
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end

  defp skip_cont(config, string, line, {:skip_token, push}, error) do
    string = push <> string
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end

  defp skip_cont(config, string, line, {:error, _error}, error) do
    skip_tokens(config, config.initial_state, string, line, string, 0, line, error, :reject, 0)
  end
end
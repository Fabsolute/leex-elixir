defmodule Leex.Runtime.Token do
  alias Leex.Runtime.Helper
  alias Leex.Token

  def token(
        config,
        state,
        string,
        line,
        token_string,
        token_len,
        token_line,
        accept_action,
        accept_len
      ) do
    case config.state.(state, string, line, token_len, accept_action, accept_len) do
      # Accepting end state, we have a token.
      {accept_action, accept_len, string, line} ->
        token_cont(
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_string, token_line)
        )

      # Accepting transition state, can take more chars.
      # Need more chars to check
      {accept_action, accept_len, [], line, state} ->
        {:more,
         {:token, state, line, token_string, accept_len, token_line, accept_action, accept_len}}

      # Take what we got
      {accept_action, accept_len, string, line, _state} ->
        token_cont(
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_string, token_line)
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
               config.module,
               # Skip eof tail in token_val.
               {:illegal, Helper.prefix(token_string, token_len)}
             }, line}
          else
            {:eof, line}
          end

        {:done, return_value, :eof}

      # No token match
      {:reject, _accept_len, token_len, string, line, _state} ->
        error =
          {token_line, config.module, {:illegal, Helper.prefix(token_string, token_len + 1)}}

        {:done, {:error, error, line}, string}

      # Use last accept match
      {accept_action, accept_len, token_len, _string, line, _state} ->
        token_string_2 = Helper.suffix(token_string, accept_len)
        line = Helper.adjust_line(token_len, accept_len, token_string_2, line)

        token_cont(
          config,
          token_string_2,
          line,
          config.action.(accept_action, accept_len, token_string, token_line)
        )
    end
  end

  defp token_cont(_config, string, line, %Token{type: :token, value: token, push_back: nil}) do
    {:done, {:ok, token, line}, string}
  end

  defp token_cont(_config, string, line, %Token{type: :token, value: token, push_back: push}) do
    string = push <> string
    {:done, {:ok, token, line}, string}
  end

  defp token_cont(_config, string, line, %Token{type: :end_token, value: token, push_back: nil}) do
    {:done, {:ok, token, line}, string}
  end

  defp token_cont(_config, string, line, %Token{type: :end_token, value: token, push_back: push}) do
    string = push <> string
    {:done, {:ok, token, line}, string}
  end

  defp token_cont(config, string, line, %Token{type: :skip_token, push_back: nil}) do
    token(config, config.initial_state, string, line, string, 0, line, :reject, 0)
  end

  defp token_cont(config, string, line, %Token{type: :skip_token, push_back: push}) do
    string = push <> string
    token(config, config.initial_state, string, line, string, 0, line, :reject, 0)
  end

  defp token_cont(config, string, line, {:error, error}) do
    {:done, {:error, {line, config.module, {:user, error}}, line}, string}
  end
end

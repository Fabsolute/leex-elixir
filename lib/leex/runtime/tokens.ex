defmodule Leex.Runtime.Tokens do
  alias Leex.Runtime.{Helper, SkipTokens}

  def tokens(
        config,
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
    case config.state.(state, string, line, token_len, accept_action, accept_len) do
      # Accepting end state, we have a token.
      {accept_action, accept_len, string, line} ->
        tokens_cont(
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_val, token_line),
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
          config,
          string,
          line,
          config.action.(accept_action, accept_len, token_val, token_line),
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
                 config.module,
                 # Skip eof tail in token_val.
                 {:illegal, Helper.prefix(token_val, token_len)}
               }, line}

            tokens == [] ->
              {:eof, line}

            true ->
              {:ok, Helper.reverse(tokens), line}
          end

        {:done, return_val, :eof}

      {:reject, _accept_len, token_len, _string, line, _state} ->
        # Skip rest of tokens.
        error =
          {line, config.module, {:illegal, Helper.prefix(token_val, token_len + 1)}}

        SkipTokens.skip_tokens(config, Helper.suffix(token_val, token_len + 1), line, error)

      {accept_action, accept_len, token_len, _string, line, _state} ->
        token = config.action.(accept_action, accept_len, token_val, token_line)
        token_val = Helper.suffix(token_val, accept_len)
        line = Helper.adjust_line(token_len, accept_len, token_val, line)
        tokens_cont(config, token_val, line, token, tokens)
    end
  end

  defp tokens_cont(config, string, line, {:token, token}, tokens) do
    tokens(
      config,
      config.initial_state,
      string,
      line,
      string,
      0,
      line,
      [token | tokens],
      :reject,
      0
    )
  end

  defp tokens_cont(config, string, line, {:token, token, push}, tokens) do
    string = push <> string

    tokens(
      config,
      config.initial_state,
      string,
      line,
      string,
      0,
      line,
      [token | tokens],
      :reject,
      0
    )
  end

  defp tokens_cont(_config, string, line, {:end_token, token}, tokens) do
    {:done, {:ok, Helper.reverse(tokens, [token]), line}, string}
  end

  defp tokens_cont(_config, string, line, {:end_token, token, push}, tokens) do
    string = push <> string
    {:done, {:ok, Helper.reverse(tokens, [token]), line}, string}
  end

  defp tokens_cont(config, string, line, :skip_token, tokens) do
    tokens(config, config.initial_state, string, line, string, 0, line, tokens, :reject, 0)
  end

  defp tokens_cont(config, string, line, {:skip_token, push}, tokens) do
    string = push <> string
    tokens(config, config.initial_state, string, line, string, 0, line, tokens, :reject, 0)
  end

  defp tokens_cont(config, string, line, {:error, error}, _tokens) do
    SkipTokens.skip_tokens(config, string, line, {line, config.module, {:user, error}})
  end
end

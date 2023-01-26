defmodule Leex.Util.Parser do
  alias Leex.Util

  @defs_head "@Definitions"
  @rule_head "@Rules"
  @code_head "@Code"

  def parse_head(ifile, state) do
    {Util.next_line(ifile, 0, state), state}
  end

  def parse_defs(ifile, {:ok, @defs_head <> _rest, line_number}, state) do
    # todo removed warning for unused characters
    parse_defs(ifile, Util.next_line(ifile, line_number, state), [], state)
  end

  def parse_defs(_, {:ok, _, line_number}, state) do
    Util.add_error({line_number, :leex, :missing_defs}, state)
  end

  def parse_defs(_, {:eof, line_number}, state) do
    Util.add_error({line_number, :leex, :missing_defs}, state)
  end

  def parse_defs(ifile, {:ok, string, line_number} = line, defs, state) do
    # This little beauty matches out a macro definition, RE's are so clear.
    regex = ~r/^[ \t]*([A-Z_][A-Za-z0-9_]*)[ \t]*=[ \t]*([^ \t\r\n]*)[ \t\r\n]*\$/u

    case Regex.run(regex, string, capture: [:list, :all_but_first]) do
      nil ->
        {line, defs, state}

      [name, def] ->
        parse_defs(ifile, Util.next_line(ifile, line_number, state), [{name, def} | defs], state)
    end
  end

  def parse_defs(_, line, defs, state), do: {line, defs, state}

  def parse_rules(ifile, {:ok, @rule_head <> _rest, line_number}, defs, state) do
    # todo removed warning for unused characters
    parse_rules(ifile, Util.next_line(ifile, line_number, state), defs, [], [], 0, state)
  end

  def parse_rules(_, {:ok, _, line_number}, _, state) do
    Util.add_error({line_number, :leex, :missing_rules}, state)
  end

  def parse_rules(_, {:eof, line_number}, _, state) do
    Util.add_error({line_number, :leex, :missing_rules}, state)
  end

  def parse_rules(ifile, line, defs, regex_actions, actions, acount, state) do
    case line do
      {:oef, _} ->
        parse_rules_end(ifile, line, regex_actions, actions, state)

      {:ok, @code_head <> _rest, _} ->
        parse_rules_end(ifile, line, regex_actions, actions, state)

      {:ok, string, line_number} ->
        case collect_rule(ifile, string, line_number) do
          {:ok, regex, action_tokens, new_line_number} ->
            {regex_action, action, new_state} =
              parse_rule(regex, line_number, action_tokens, defs, acount, state)

            parse_rules(
              ifile,
              Util.next_line(ifile, new_line_number, state),
              defs,
              [regex_action | regex_actions],
              [action | actions],
              acount + 1,
              new_state
            )

          {:error, error} ->
            Util.add_error(error, state)
        end
    end
  end

  defp parse_rule(regex, line_number, [{:dot, _}], defs, acount, state) do
    case Util.Regex.parse_rule_regexp(regex, defs, state) do
      {:ok, regex} ->
        {:ok, {regex, acount}, {acount, :empty_action}, state}

      {:error, error} ->
        Util.add_error({line_number, :leex, error}, state)
    end
  end

  defp parse_rule(regex, line_number, action_tokens, defs, acount, state) do
    case Util.Regex.parse_rule_regexp(regex, defs, state) do
      {:ok, regex} ->
        [token_val, token_len, token_line] =
          Enum.map(["token_val", "token_len", "token_line"], &var_used(&1, action_tokens))

        # Check for token variables.
        {:ok, {regex, acount}, {acount, action_tokens, token_val, token_len, token_line}, state}

      {:error, e} ->
        Util.add_error({line_number, :leex, e}, state)
    end
  end

  defp parse_rules_end(_, {:ok, _, line_number}, [], [], state) do
    Util.add_error({line_number, :leex, :empty_rules}, state)
  end

  defp parse_rules_end(_, {:eof, line_number}, [], [], state) do
    Util.add_error({line_number, :leex, :empty_rules}, state)
  end

  defp parse_rules_end(_, line, regex_actions, actions, state) do
    #  Must be *VERY* careful to put rules in correct order!
    {line, Enum.reverse(regex_actions), Enum.reverse(actions), state}
  end

  defp collect_rule(ifile, string, line_number) do
    {regex, rest} = :string.take(string, " \t\r\n", true)

    case collect_action(ifile, rest, line_number, []) do
      {:ok, [{:->, _} | tokens], line_number} -> {:ok, regex, tokens, line_number}
      {:ok, _, _} -> {:error, {line_number, :leex, :bad_rule}}
      {:eof, line_number} -> {:error, {line_number, :leex, :bad_rule}}
      {:error, error, _} -> {:error, error}
    end
  end

  defp collect_action(_ifile, {:error, _}, line_number, _cont) do
    {:error, {line_number, :leex, :cannot_parse}, :ignored_end_line}
  end

  defp collect_action(ifile, string, line_number, continuation) do
    # todo this should be work for elixir code not erlang
    case :erl_scan.tokens(continuation, string, line_number) do
      {:done, {:ok, tokens, _}, _} ->
        {:ok, tokens, line_number}

      {:done, {:eof, _}, _} ->
        {:eof, line_number}

      {:done, {:error, error, _}, _} ->
        {:error, error, line_number}

      {:more, continuation} ->
        collect_action(ifile, IO.gets(ifile, :leex), line_number + 1, continuation)
    end
  end

  defp var_used(name, tokens) do
    # todo this should be work for elixir code not erlang
    List.keyfind(tokens, name, 3) != nil
  end
end

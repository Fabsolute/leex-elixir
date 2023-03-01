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
    regex = ~r/^[ \t]*([A-Z_][A-Za-z0-9_]*)[ \t]*=[ \t]*([^ \t\r\n]*)[ \t\r\n]*$/u

    case Regex.run(regex, string, capture: :all_but_first) do
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
          {:ok, regex, code, action_tokens, new_line_number} ->
            {regex_action, action, new_state} =
              parse_rule(regex, line_number, code, action_tokens, defs, acount, state)
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

  def parse_code(ifile, {:ok, @code_head <> _string, code_line_number}, state) do
    # todo removed warning for unused characters
    # hmm ?
    {:ok, code_position} = :file.position(ifile, :cur)
    end_code_line_number = Util.count_lines(ifile, code_line_number, state)
    new_code_line_number = end_code_line_number - code_line_number
    {{code_line_number, code_position, new_code_line_number}, state}
  end

  def parse_code(_, {:ok, _, line_number}, state) do
    Util.add_error({line_number, :leex, :missing_code}, state)
  end

  def parse_code(_, {:eof, line_number}, state) do
    Util.add_error({line_number, :leex, :missing_code}, state)
  end

  defp parse_rule(regex, line_number, _code, [{:__block__, [], []}], defs, acount, state) do
    case Util.Regex.parse_rule_regexp(regex, defs) do
      {:ok, regex} ->
        {{regex, acount}, {acount, :empty_action}, state}

      {:error, error} ->
        Util.add_error({line_number, :leex, error}, state)
    end
  end

  defp parse_rule(regex, line_number, code, action_tokens, defs, acount, state) do
    case Util.Regex.parse_rule_regexp(regex, defs) do
      {:ok, regex} ->
        [token_val, token_len, token_line] =
          Enum.map(["token_val", "token_len", "token_line"], &var_used(code, &1))

        # Check for token variables.
        {{regex, acount}, {acount, action_tokens, token_val, token_len, token_line}, state}

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
    {regex, rest} = Util.string_take(string, ' \t\r\n', true)

    case collect_action(ifile, rest, line_number) do
      {:ok, tokens, code, line_number} -> {:ok, regex, code, tokens, line_number}
      {:error, error} -> {:error, error}
    end
  end

  defp collect_action(_ifile, {:error, _}, line_number) do
    {:error, {line_number, :leex, :cannot_parse}, :ignored_end_line}
  end

  defp collect_action(ifile, code, line_number) do
    case Code.string_to_quoted("def #{code}") do
      {:ok, {:def, _, [[do: context]]}} ->
        {:ok, context, code, line_number}

      {:error, error} ->
        case IO.gets(ifile, :leex) do
          :eof -> {:error, error}
          {:error, error_2} -> {:error, {error, error_2}}
          content -> collect_action(ifile, "#{code}\n#{content}", line_number + 1)
        end
    end
  end

  defp var_used(code, name) do
    Regex.match?(~r/(?<![\w:])#{name}(?![\w(])/, code)
  end
end

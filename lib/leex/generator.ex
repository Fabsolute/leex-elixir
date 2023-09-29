defmodule Leex.Generator do
  alias Leex.Util
  alias Leex.Generator

  @spec generate_functions(
          list({String.t(), Macro.t()}),
          list({String.t(), String.t()})
        ) :: Macro.t()
  def generate_functions(rules, defs) do
    {{dfa, dfa_first}, actions} = get_actions(rules, defs)

    public_functions = Generator.Public.generate_public(dfa_first)
    action_functions = Generator.Action.generate_actions(actions)
    dfa_functions = Generator.DFA.generate_dfas(dfa)

    quote do
      unquote(public_functions)
      unquote(action_functions)
      unquote(dfa_functions)
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
    |> then(fn {regex_actions, actions} ->
      dfa = Leex.DFA.make_dfa(regex_actions)

      actions =
        Enum.map(actions, fn
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

      {dfa, actions}
    end)
  end
end

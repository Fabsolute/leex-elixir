defmodule Leex.Generator.Action do
  @opaque action :: {integer, :empty_action} | {integer, Macro.t(), list(atom)}

  def generate_actions(actions) do
    for {action, code, [token_line, yy_tcs, token_len]} <- actions do
      token_val_definition =
        if yy_tcs != :_ do
          quote do
            unquote(Macro.var(:token_val, nil)) =
              Runtime.Helper.prefix(
                unquote(Macro.var(yy_tcs, nil)),
                unquote(Macro.var(token_len, nil))
              )
          end
        end

      quote do
        defp leex_action(
               unquote(action),
               unquote(Macro.var(token_len, nil)),
               unquote(Macro.var(yy_tcs, nil)),
               unquote(Macro.var(token_line, nil))
             ) do
          unquote(token_val_definition)
          unquote(code)
        end
      end
    end
  end
end

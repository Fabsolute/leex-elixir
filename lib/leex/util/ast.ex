defmodule Leex.Util.AST do
  defguardp is_literal(val)
            when is_atom(val) or
                   is_number(val) or
                   is_bitstring(val)

  def var_used(token, _name) when is_literal(token), do: false
  def var_used({name, _line, nil}, name), do: true
  def var_used(token, name) when is_tuple(token), do: var_used(Tuple.to_list(token), name)

  def var_used([], _name), do: false

  def var_used([h | tail], name) do
    if var_used(h, name) do
      true
    else
      var_used(tail, name)
    end
  end
end

defmodule Leex.Util.OrdSet do
  def new(), do: :ordsets.new()
  def put(set, elem), do: :ordsets.add_element(elem, set)
  def union(set_1, set_2), do: :ordsets.union(set_1, set_2)
  def member?(set, elem), do: :ordsets.is_element(elem, set)
end

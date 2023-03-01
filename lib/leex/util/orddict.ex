defmodule OrdDict do
  def new(), do: :orddict.new()
  def store(dict, key, value), do: :orddict.store(key, value, dict)
end

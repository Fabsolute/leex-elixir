defmodule Leex.Runtime.Helper do
  def adjust_line(token_len, token_len, _string, line), do: line

  def adjust_line(token_len, accept_len, <<?\n, string::binary>>, line) do
    adjust_line(token_len - 1, accept_len, string, line - 1)
  end

  def adjust_line(token_len, accept_len, <<_, string::binary>>, line) do
    adjust_line(token_len - 1, accept_len, string, line)
  end

  def reverse(list), do: Enum.reverse(list)
  def reverse(list, tail), do: Enum.reverse(list, tail)
  def prefix(string, n), do: String.slice(string, 0..(n - 1))
  def suffix(string, n), do: String.slice(string, n..-1)
end

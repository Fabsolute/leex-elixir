defmodule Example do
  use Leex

  defd("INT", "[0-9]+")
  defd("ATOM", ":[a-z_]+")

  skip "[\s\t\n\r]+"

  defr "{INT}" do
    token({:int, token_line, String.to_integer(token_val)})
  end

  defr "{ATOM}" do
    token({:atom, token_line, to_atom(token_val)})
  end

  defr "\\[" do
    token({:"[", token_line})
  end

  defr "\\]" do
    token({:"]", token_line})
  end

  defr "," do
    val = "what about string literal #{token_line}"
    token({:",", val})
  end

  defp to_atom(<<?:, string::binary>>) do
    String.to_atom(string)
  end
end

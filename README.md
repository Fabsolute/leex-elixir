# Leex

Leex is an open source implementation of `:erlang.leex`.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `leex` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:leex, "~> 0.1.0"}
  ]
end
```

Usage
```elixir
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
    token({:",", token_line})
  end

  defp to_atom(<<?:, string::binary>>) do
    String.to_atom(string)
  end
end
```

# Definition

You can create a definition with `defd` function. 
For instance: `defd("INT", "[0-9]+")`

# Rule

You can create a rule with `defr` function.
For instance: 
```elixir
defr "0x{INT}" do
    token({:hex_number, token_line, String.to_integer(token_val)})
end
```

# Skip

You can create a ignored tokens with `skip` function.
For instance `skip "[\s\t\n\r]+"`

## Variables

`token_line`: Line of token

`token_val`: Value of token

`token_len`: Length of token


Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/leex>.

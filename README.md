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

  "INT" <~ "[0-9]+"
  "ATOM" <~ ":[a-z_]+"
  "WHITESPACE" <~ "[\s\t\n\r]"

  "{INT}" ~> {:token, {:int, token_line, String.to_integer(token_val)}}
  "{ATOM}" ~> {:token, {:atom, token_line, to_atom(token_val)}}
  "\\[" ~> {:token, {:"[", token_line}}
  "\\]" ~> {:token, {:"]", token_line}}
  "," ~> {:token, {:",", token_line}}
  "{WHITESPACE}+" ~> :skip_token

  def to_atom(<<?:, string::binary>>) do
    String.to_atom(string)
  end
end
```

# Definition operator <~

You can create a definition with `<~` operator. 
For instance: `"INT" <~ "[0-9]+"`

# Rule operator ~>

You can create a rule with `~>` operator.
For instance: `"0x{INT}" ~> {:token, {:hex_number, token_line, String.to_integer(token_val)}}`

## Variables

`token_line`: Line of token
`token_val`: Value of token
`token_len`: Length of token


Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/leex>.

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

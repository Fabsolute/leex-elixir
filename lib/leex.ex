defmodule Leex do
  alias Leex.Util.Core

  @token :token
  @skip_token :skip_token
  @end_token :end_token

  defmacro __using__(_opts) do
    quote do
      @defs []
      @rules []
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro defd(name, definition) when is_bitstring(definition) do
    quote do
      @defs @defs ++ [{unquote(name), unquote(definition)}]
    end
  end

  defmacro skip(rule) when is_bitstring(rule) do
    skip_token = @skip_token

    quote do
      @rules @rules ++ [{unquote(rule), unquote(skip_token)}]
    end
  end

  defmacro defr(rule, do: expr) when is_bitstring(rule) do
    quote do
      @rules @rules ++ [{unquote(rule), unquote(expr |> Macro.escape())}]
    end
  end

  defmacro skip_token(push_back) do
    {@skip_token, push_back}
  end

  defmacro token(selection, push_back \\ nil) do
    if push_back == nil do
      {@token, selection}
    else
      {@token, selection, push_back}
    end
  end

  defmacro end_token(selection, push_back \\ nil) do
    if push_back == nil do
      {@end_token, selection}
    else
      {@end_token, selection, push_back}
    end
  end

  defmacro __before_compile__(env) do
    rules = Module.get_attribute(env.module, :rules)
    defs = Module.get_attribute(env.module, :defs)

    Module.delete_attribute(env.module, :rules)
    Module.delete_attribute(env.module, :defs)

    Core.generate_functions(rules, defs)
  end
end

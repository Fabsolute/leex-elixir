defmodule Leex do
  alias Leex.Generator
  alias Leex.Token

  defmacro defd(name, definition) when is_bitstring(name) and is_bitstring(definition) do
    quote do
      @defs {unquote(name), unquote(definition)}
    end
  end

  defmacro skip(rule) when is_bitstring(rule) do
    quote do
      @rules {unquote(rule), Token.skip_token() |> Macro.escape()}
    end
  end

  defmacro defr(rule, do: expr) when is_bitstring(rule) do
    quote do
      @rules {unquote(rule), unquote(expr |> Macro.escape())}
    end
  end

  defmacro skip_token(push_back \\ nil) do
    quote do
      Token.skip_token(unquote(push_back))
    end
  end

  defmacro token(selection, push_back \\ nil) do
    quote do
      Token.token(unquote(selection), unquote(push_back))
    end
  end

  defmacro end_token(selection, push_back \\ nil) do
    quote do
      Token.end_token(unquote(selection), unquote(push_back))
    end
  end

  defmacro __using__([]) do
    quote do
      Module.register_attribute(__MODULE__, :defs, accumulate: true)
      Module.register_attribute(__MODULE__, :rules, accumulate: true)

      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(env) do
    rules = Module.get_attribute(env.module, :rules)
    defs = Module.get_attribute(env.module, :defs)

    Module.delete_attribute(env.module, :rules)
    Module.delete_attribute(env.module, :defs)

    Generator.generate_functions(rules, defs)
  end
end

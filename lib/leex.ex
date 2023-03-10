defmodule Leex do
  alias Leex.Util.Core

  defmacro __using__(_opts) do
    quote do
      @defs []
      @rules []
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro a <~ b when is_bitstring(a) do
    quote do
      @defs @defs ++ [{unquote(a), unquote(b)}]
    end
  end

  defmacro a ~> b when is_bitstring(a) do
    block = Macro.escape(b)

    quote do
      @rules @rules ++ [{unquote(a), unquote(block)}]
    end
  end

  defmacro __before_compile__(env) do
    rules = Module.get_attribute(env.module, :rules)
    defs = Module.get_attribute(env.module, :defs)

    Module.delete_attribute(env.module, :rules)
    Module.delete_attribute(env.module, :defs)

    {included_functions, action_functions, dfa_functions} = Core.generate_functions(rules, defs)

    quote do
      unquote(included_functions)
      unquote(action_functions)
      unquote(dfa_functions)
    end
  end
end

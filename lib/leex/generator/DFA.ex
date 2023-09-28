defmodule Leex.Generator.DFA do
  alias Leex.Util.Trans
  alias Leex.DFA

  def generate_dfas(dfa) when is_list(dfa) do
    states = Enum.map(dfa, &generate/1)

    quote do
      unquote(states)
      defp leex_state(s, ics, line, tlen, action, alen), do: {action, alen, tlen, ics, line, s}
    end
  end

  defp generate(%DFA{no: no, trans: [], accept: {:accept, accept}}) do
    quote do
      defp leex_state(unquote(no), ics, line, tlen, _, _) do
        {unquote(accept), tlen, ics, line}
      end
    end
  end

  defp generate(%DFA{no: no, trans: trans, accept: {:accept, accept}}) do
    accept_state_ast =
      trans |> Trans.pack_trans() |> Enum.map(&generate_accept(no, accept, &1))

    quote do
      unquote(accept_state_ast)

      defp leex_state(unquote(no), ics, line, tlen, _, _) do
        {unquote(accept), tlen, ics, line, unquote(no)}
      end
    end
  end

  defp generate(%DFA{no: no, trans: trans, accept: :noaccept}) do
    noaccept_state_ast = trans |> Trans.pack_trans() |> Enum.map(&generate_noaccept(no, &1))

    quote do
      unquote(noaccept_state_ast)

      defp leex_state(unquote(no), ics, line, tlen, action, alen) do
        {action, alen, tlen, ics, line, unquote(no)}
      end
    end
  end

  # region Accept

  defp generate_accept(next_state, action, {{char, :maxchar}, state}) do
    generate_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> generate_accept_max(next_state, char)
  end

  defp generate_accept(next_state, action, {{char_1, char_2}, state}) do
    generate_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> generate_accept_range(next_state, char_1, char_2)
  end

  defp generate_accept(next_state, action, {?\n, state}) do
    generate_accept_body(
      state,
      quote do
        line + 1
      end,
      action
    )
    |> generate_accept_one(next_state, ?\n)
  end

  defp generate_accept(next_state, action, {char, state}) do
    generate_accept_body(state, Macro.var(:line, __MODULE__), action)
    |> generate_accept_one(next_state, char)
  end

  defp generate_accept_max(body, state, min) do
    generate_max(body, state, min, Macro.var(:_, __MODULE__), Macro.var(:_, __MODULE__))
  end

  defp generate_accept_range(body, state, min, max) do
    generate_range(
      body,
      state,
      min,
      max,
      Macro.var(:_, __MODULE__),
      Macro.var(:_, __MODULE__)
    )
  end

  defp generate_accept_one(body, state, char) do
    generate_one(body, state, char, Macro.var(:_, __MODULE__), Macro.var(:_, __MODULE__))
  end

  defp generate_accept_body(next, line, action) do
    generate_body(next, line, action, Macro.var(:tlen, __MODULE__))
  end

  # endregion

  # region No Accept

  defp generate_noaccept(next_state, {{char, :maxchar}, state}) do
    generate_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> generate_noaccept_max(next_state, char)
  end

  defp generate_noaccept(next_state, {{char_1, char_2}, state}) do
    generate_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> generate_noaccept_range(next_state, char_1, char_2)
  end

  defp generate_noaccept(next_state, {?\n, state}) do
    generate_noaccept_body(
      state,
      quote do
        line + 1
      end
    )
    |> generate_noaccept_one(next_state, ?\n)
  end

  defp generate_noaccept(next_state, {char, state}) do
    generate_noaccept_body(state, Macro.var(:line, __MODULE__))
    |> generate_noaccept_one(next_state, char)
  end

  defp generate_noaccept_max(body, state, min) do
    generate_max(body, state, min, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  defp generate_noaccept_range(body, state, min, max) do
    generate_range(
      body,
      state,
      min,
      max,
      Macro.var(:action, __MODULE__),
      Macro.var(:alen, __MODULE__)
    )
  end

  defp generate_noaccept_one(body, state, char) do
    generate_one(body, state, char, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  defp generate_noaccept_body(next, line) do
    generate_body(next, line, Macro.var(:action, __MODULE__), Macro.var(:alen, __MODULE__))
  end

  # endregion

  defp generate_max(body, state, min, action, alen) do
    quote do
      defp leex_state(
             unquote(state),
             <<c, ics::binary>>,
             line,
             tlen,
             unquote(action),
             unquote(alen)
           )
           when c >= unquote(min) do
        unquote(body)
      end
    end
  end

  defp generate_range(body, state, min, max, action, alen) do
    quote do
      defp leex_state(
             unquote(state),
             <<c, ics::binary>>,
             line,
             tlen,
             unquote(action),
             unquote(alen)
           )
           when c >= unquote(min) and c <= unquote(max) do
        unquote(body)
      end
    end
  end

  defp generate_one(body, state, char, action, alen) do
    quote do
      defp leex_state(
             unquote(state),
             <<unquote(char), ics::binary>>,
             line,
             tlen,
             unquote(action),
             unquote(alen)
           ) do
        unquote(body)
      end
    end
  end

  defp generate_body(next, line, action, alen) do
    quote do
      leex_state(
        unquote(next),
        ics,
        unquote(line),
        tlen + 1,
        unquote(action),
        unquote(alen)
      )
    end
  end
end

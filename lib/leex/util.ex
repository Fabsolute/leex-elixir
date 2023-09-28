defmodule Leex.Util do
  alias Leex.NFA

  def parse_error(error) do
    throw({:parse_error, error})
  end

  def special_char(c) do
    c in [?^, ?., ?[, ?$, ?(, ?), ?|, ?*, ?+, ??, ?\\]
  end

  def string_between(string_1, string_2) do
    String.slice(string_1, 0, String.length(string_1) - String.length(string_2))
  end

  def escape_char(?n), do: ?\n
  def escape_char(?r), do: ?\r
  def escape_char(?t), do: ?\t
  def escape_char(?v), do: ?\v
  def escape_char(?b), do: ?\b
  def escape_char(?f), do: ?\f
  def escape_char(?e), do: ?\e
  def escape_char(?s), do: ?\s
  def escape_char(?d), do: ?\d
  def escape_char(c), do: c

  def epsilon_trans(firsts), do: Enum.map(firsts, fn f -> {:epsilon, f} end)

  def pack_cc(char_class) do
    List.foldl(char_class, :ordsets.new(), fn
      {:range, cf, cl}, set -> :ordsets.add_element({cf, cl}, set)
      c, set -> :ordsets.add_element({c, c}, set)
    end)
    |> Enum.sort()
    |> pack_crs()
  end

  def comp_class(comp_class) do
    pack_cc(comp_class)
    |> comp_crs(0)
  end

  def eclosure(states, nfa), do: eclosure(states, nfa, :ordsets.new())

  def move(states, cr, nfa) do
    for n <- states do
      for {crs, state} <- elem(nfa, n - 1).edges, crs != :epsilon, in_crs(cr, crs), do: state
    end
    |> List.flatten()
  end

  defp in_crs({c1, c2}, [{c3, c4} | _crs]) when c1 >= c3 and c2 <= c4, do: true
  defp in_crs(cr, [cr | _crs]), do: true
  defp in_crs(cr, [_ | crs]), do: in_crs(cr, crs)
  defp in_crs(_cr, []), do: false

  defp eclosure([state | states], nfa, eclosure) do
    %NFA{edges: edges} = elem(nfa, state - 1)

    eclosure(
      for({:epsilon, n} <- edges, !:ordsets.is_element(n, eclosure), do: n) ++ states,
      nfa,
      :ordsets.add_element(state, eclosure)
    )
  end

  defp eclosure([], _, eclosure), do: eclosure

  defp comp_crs([{0, c2} | crs], 0), do: comp_crs(crs, c2 + 1)

  defp comp_crs([{c1, c2} | crs], last) do
    [{last, c1 - 1} | comp_crs(crs, c2 + 1)]
  end

  defp comp_crs([], last), do: [{last, :maxchar}]

  defp pack_crs([{c1, c2} = cr, {c3, c4} | crs]) when c1 <= c3 and c2 >= c4 do
    pack_crs([cr | crs])
  end

  defp pack_crs([{c1, c2}, {c3, c4} | crs]) when c2 >= c3 and c2 < c4 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([{c1, c2}, {c3, c4} | crs]) when c2 + 1 == c3 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([cr | crs]), do: [cr | pack_crs(crs)]
  defp pack_crs([]), do: []
end

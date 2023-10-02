defmodule Leex.DFA do
  alias Leex.Util
  alias Leex.NFA

  defstruct no: nil, nfa: [], trans: [], accept: :noaccept

  def make_dfa(regex_actions) do
    {nfa, nfa_first} = NFA.build_combined_nfa(regex_actions)
    {dfa, dfa_first} = build_dfa(nfa, nfa_first)
    minimise_dfa(dfa, dfa_first)
  end

  defp build_dfa(nfa, nfa_first_state) do
    dfa = %__MODULE__{no: 0, nfa: Util.eclosure([nfa_first_state], nfa)}
    {build_dfa([dfa], 1, [], nfa), 0}
  end

  defp build_dfa([unmarked | unmarkeds], next_state, markeds, nfa) do
    {trans, unmarkeds, next_state} =
      build_dfa(unmarked.nfa, unmarkeds, next_state, [], [unmarked | markeds], nfa)

    marked = %{unmarked | trans: trans, accept: accept(unmarked.nfa, nfa)}
    build_dfa(unmarkeds, next_state, [marked | markeds], nfa)
  end

  defp build_dfa([], _, markeds, _), do: markeds

  defp build_dfa(states, unmarkeds, next_state, transitions, markeds, nfa) do
    for s <- states do
      for {crs, _st} <- elem(nfa, s - 1).edges, crs != :epsilon do
        for cr <- crs, do: cr
      end
    end
    |> List.flatten()
    |> Enum.uniq()
    |> Enum.sort()
    |> disjoint_crs()
    |> build_dfa(states, unmarkeds, next_state, transitions, markeds, nfa)
  end

  defp build_dfa([cr | crs], states, unmarkeds, next_state, transitions, markeds, nfa) do
    case Util.eclosure(Util.move(states, cr, nfa), nfa) do
      [] ->
        build_dfa(crs, states, unmarkeds, next_state, transitions, markeds, nfa)

      s ->
        case dfa_state_exist(s, unmarkeds, markeds) do
          {:yes, t} ->
            build_dfa(
              crs,
              states,
              unmarkeds,
              next_state,
              :orddict.store(cr, t, transitions),
              markeds,
              nfa
            )

          :no ->
            unmarked = %__MODULE__{no: next_state, nfa: s}

            build_dfa(
              crs,
              states,
              [unmarked | unmarkeds],
              next_state + 1,
              :orddict.store(cr, next_state, transitions),
              markeds,
              nfa
            )
        end
    end
  end

  defp build_dfa([], _, unmarkeds, next_state, transitions, _, _) do
    {transitions, unmarkeds, next_state}
  end

  defp minimise_dfa(dfa, dfa_first) do
    case min_dfa(dfa) do
      {dfa, []} ->
        {dfa, rs} = pack_dfa(dfa)
        {min_update(dfa, rs), min_use(dfa_first, rs)}

      {dfa, rs} ->
        minimise_dfa(min_update(dfa, rs), min_use(dfa_first, rs))
    end
  end

  defp min_dfa(dfa), do: min_dfa(dfa, [], [])

  defp min_dfa([dfa | rest_dfa], rewrites, mini_dfa) do
    {rest_dfa, rewrites} = min_delete(rest_dfa, dfa.trans, dfa.accept, dfa.no, rewrites, [])
    min_dfa(rest_dfa, rewrites, [dfa | mini_dfa])
  end

  defp min_dfa([], rewrites, mini_dfa), do: {mini_dfa, rewrites}

  defp min_update(dfa, rewrites) do
    Enum.map(dfa, fn d ->
      %{d | trans: min_update_trans(d.trans, rewrites)}
    end)
  end

  defp min_update_trans(transitions, rewrites) do
    Enum.map(transitions, fn {c, s} ->
      {c, min_use(s, rewrites)}
    end)
  end

  defp min_use(old, [{old, new} | _]), do: new
  defp min_use(old, [_ | reds]), do: min_use(old, reds)
  defp min_use(old, []), do: old

  defp min_delete(
         [%__MODULE__{no: no, trans: trans, accept: accept} | dfa],
         trans,
         accept,
         new_next_state,
         rewrites,
         mini_dfa
       ) do
    min_delete(dfa, trans, accept, new_next_state, [{no, new_next_state} | rewrites], mini_dfa)
  end

  defp min_delete([dfa | rest_dfa], transition, accept, new_next_state, rewrites, mini_dfa) do
    min_delete(rest_dfa, transition, accept, new_next_state, rewrites, [dfa | mini_dfa])
  end

  defp min_delete([], _, _, _, rewrites, mini_dfa), do: {mini_dfa, rewrites}

  defp accept([state | states], nfa) do
    case elem(nfa, state - 1) do
      %NFA{accept: {:accept, a}} -> {:accept, a}
      %NFA{accept: :noaccept} -> accept(states, nfa)
    end
  end

  defp accept([], _), do: :noaccept

  defp disjoint_crs([{_c1, c2} = cr1, {c3, _c4} = cr2 | crs]) when c2 < c3 do
    [cr1 | disjoint_crs([cr2 | crs])]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 == c3 do
    [{c1, c2} | disjoint_crs(:ordsets.add_element({c2 + 1, c4}, crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 and c2 >= c3 and c2 < c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.union([{c3, c2}, {c2 + 1, c4}], crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 and c2 == c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.add_element({c3, c4}, crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 and c2 > c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.union([{c3, c4}, {c4 + 1, c2}], crs))]
  end

  defp disjoint_crs([cr | crs]), do: [cr | disjoint_crs(crs)]
  defp disjoint_crs([]), do: []

  defp dfa_state_exist(state, unmarkeds, markeds) do
    case Enum.find(unmarkeds, &(&1.nfa == state)) do
      %__MODULE__{no: t} ->
        {:yes, t}

      nil ->
        case Enum.find(markeds, &(&1.nfa == state)) do
          %__MODULE__{no: t} -> {:yes, t}
          nil -> :no
        end
    end
  end

  defp pack_dfa(dfa), do: pack_dfa(dfa, 0, [], [])

  defp pack_dfa([dfa | rest_dfa], new_next_state, rewrites, packed_dfa) do
    pack_dfa(rest_dfa, new_next_state + 1, [{dfa.no, new_next_state} | rewrites], [
      %{dfa | no: new_next_state} | packed_dfa
    ])
  end

  defp pack_dfa([], _, rewrites, packed_dfa), do: {packed_dfa, rewrites}
end

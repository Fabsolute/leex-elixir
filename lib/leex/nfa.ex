defmodule Leex.NFA do
  defstruct no: nil, edges: [], accept: :noaccept
  
  alias Leex.Util

  def build_combined_nfa(regex_actions) do
    {nfa, first, free} = build_nfa_list(regex_actions, [], [], 1)
    state = %__MODULE__{no: free, edges: Util.epsilon_trans(first)}
    {List.to_tuple(Enum.sort_by([state | nfa], & &1.no)), free}
  end

  defp build_nfa_list([{regex, action} | regex_actions], nfa, firsts, free) do
    {nfa_1, free, first} = build_nfa(regex, free, action)
    build_nfa_list(regex_actions, nfa_1 ++ nfa, [first | firsts], free)
  end

  defp build_nfa_list([], nfa, firsts, free), do: {nfa, Enum.reverse(firsts), free}

  defp build_nfa(regex, state, action) do
    {nfa, next_state, end_state} = build_nfa(regex, state + 1, state, [])
    {[%__MODULE__{no: end_state, accept: {:accept, action}} | nfa], next_state, state}
  end

  # %% build_nfa(RegExp, NextState, FirstState, NFA) -> {NFA,NextState,EndState}.
  defp build_nfa({:alt, regex}, next_state, first_state, nfa) do
    build_nfa_alt(regex, next_state, first_state, nfa)
  end

  defp build_nfa({:seq, regex}, next_state, first_state, nfa) do
    build_nfa_seq(regex, next_state, first_state, nfa)
  end

  defp build_nfa({:kclosure, regex}, next_state, first_state, nfa) do
    {nfa, next_state_2, end_state_2} = build_nfa(regex, next_state + 1, next_state, nfa)
    end_state = next_state_2

    {[
       %__MODULE__{no: first_state, edges: [{:epsilon, next_state}, {:epsilon, end_state}]},
       %__MODULE__{no: end_state_2, edges: [{:epsilon, next_state}, {:epsilon, end_state}]}
       | nfa
     ], next_state + 1, end_state}
  end

  defp build_nfa({:pclosure, regex}, next_state, first_state, nfa) do
    {nfa, next_state_2, end_state_2} = build_nfa(regex, next_state + 1, next_state, nfa)
    end_state = next_state_2

    {[
       %__MODULE__{no: first_state, edges: [{:epsilon, next_state}]},
       %__MODULE__{no: end_state_2, edges: [{:epsilon, next_state}, {:epsilon, end_state}]}
       | nfa
     ], next_state_2 + 1, end_state}
  end

  defp build_nfa({:optional, regex}, next_state, first_state, nfa) do
    {nfa, next_state_2, end_state_2} = build_nfa(regex, next_state + 1, next_state, nfa)
    end_state = next_state_2

    {[
       %__MODULE__{no: first_state, edges: [{:epsilon, next_state}, {:epsilon, end_state}]},
       %__MODULE__{no: end_state_2, edges: [{:epsilon, end_state}]} | nfa
     ], next_state_2 + 1, end_state}
  end

  defp build_nfa({:char_class, regex}, next_state, first_state, nfa) do
    {[%__MODULE__{no: first_state, edges: [{Util.pack_cc(regex), next_state}]} | nfa],
     next_state + 1, next_state}
  end

  defp build_nfa({:comp_class, regex}, next_state, first_state, nfa) do
    {[%__MODULE__{no: first_state, edges: [{Util.comp_class(regex), next_state}]} | nfa],
     next_state + 1, next_state}
  end

  defp build_nfa({:lit, regex}, next_state, first_state, nfa) do
    build_nfa_lit(regex, next_state, first_state, nfa)
  end

  defp build_nfa(:epsilon, next_state, first_state, nfa) do
    {[%__MODULE__{no: first_state, edges: [{:epsilon, next_state}]} | nfa], next_state + 1,
     next_state}
  end

  defp build_nfa_alt([regex], next_state, first_state, nfa),
    do: build_nfa(regex, next_state, first_state, nfa)

  defp build_nfa_alt([regex | regexes], next_state, first_state, nfa) do
    {nfa, next_state_2, end_state_2} = build_nfa(regex, next_state + 1, next_state, nfa)
    {nfa, next_state_3, end_state_3} = build_nfa_alt(regexes, next_state_2 + 1, next_state_2, nfa)
    end_state = next_state_3

    {[
       %__MODULE__{no: first_state, edges: [{:epsilon, next_state}, {:epsilon, next_state_2}]},
       %__MODULE__{no: end_state_2, edges: [{:epsilon, end_state}]},
       %__MODULE__{no: end_state_3, edges: [{:epsilon, end_state}]} | nfa
     ], next_state_3 + 1, end_state}
  end

  defp build_nfa_seq(regexes, next_state, first_state, nfa) do
    List.foldl(
      regexes,
      {nfa, next_state, first_state},
      fn regex, {nfa, next_state, first_state} ->
        build_nfa(regex, next_state, first_state, nfa)
      end
    )
  end

  defp build_nfa_lit(regex, next_state, first_state, nfa) do
    List.foldl(regex, {nfa, next_state, first_state}, fn c, {nfa, next_state, first_state} ->
      {[%__MODULE__{no: first_state, edges: [{[{c, c}], next_state}]} | nfa], next_state + 1,
       next_state}
    end)
  end
end

defmodule Leex.Util.Trans do
  def pack_trans(trans), do: pack_trans(trans, [])

  defp pack_trans([{{char, char}, state} | trans], packed) do
    if Enum.member?(packed, {char, state}) do
      pack_trans(trans, packed)
    else
      pack_trans(trans, [{char, state} | packed])
    end
  end

  defp pack_trans([{{char, ?\n}, state} | trans], packed) do
    pack_trans([{{char, ?\n - 1}, state} | trans], [{?\n, state} | packed])
  end

  defp pack_trans([{{?\n, char}, state} | trans], packed) do
    pack_trans([{{?\n + 1, char}, state} | trans], [{?\n, state} | packed])
  end

  defp pack_trans([{{char_1, char_2}, state} | trans], packed)
       when char_1 < ?\n and char_2 > ?\n do
    pack_trans([{{char_1, ?\n - 1}, state}, {{?\n + 1, char_2}, state} | trans], [
      {?\n, state} | packed
    ])
  end

  defp pack_trans([{{char_1, char_2}, state} | trans], packed) when char_2 == char_1 + 1 do
    pack_trans(trans, [{char_1, state}, {char_2, state} | packed])
  end

  defp pack_trans([trans | rest_trans], packed) do
    pack_trans(rest_trans, packed ++ [trans])
  end

  defp pack_trans([], packed), do: packed
end

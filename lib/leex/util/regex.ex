defmodule Leex.Util.Regex do
  alias Leex.Util
  require Util

  def parse_rule_regexp(regex, [{name, def} | defs]) do
    regex
    |> String.replace("{#{name}}", def)
    |> parse_rule_regexp(defs)
  end

  def parse_rule_regexp(regex, []) do
    case re_parse(regex) do
      {:ok, regex} -> {:ok, regex}
      {:error, error} -> {:error, {:regexp, error}}
    end
  end

  defp re_parse(string) do
    try do
      case re_reg(string, 0) do
        {regex, _, ""} -> {:ok, regex}
        {_, _, <<c::utf8, _rest::binary>>} -> {:error, {:illegal_char, [c]}}
      end
    catch
      {:parse_error, error} -> {:error, error}
    end
  end

  defp re_reg(string, sub_number), do: re_alt(string, sub_number)

  defp re_alt(string, sub_number) do
    {regex, sub_number, string} = re_seq(string, sub_number)

    case re_alt1(string, sub_number) do
      {[], sub_number, string} -> {regex, sub_number, string}
      {regex2, sub_number, string} -> {{:alt, [regex | regex2]}, sub_number, string}
    end
  end

  defp re_alt1(<<?|, string::binary>>, sub_number) do
    {regex, sub_number, string} = re_seq(string, sub_number)
    {regex2, sub_number, string} = re_alt1(string, sub_number)
    {[regex | regex2], sub_number, string}
  end

  defp re_alt1(string, sub_number), do: {[], sub_number, string}

  defp re_seq(string, sub_number) do
    case re_seq1(string, sub_number) do
      {[], sub_number, string} -> {:epsilon, sub_number, string}
      {[regex], sub_number, string} -> {regex, sub_number, string}
      {regex, sub_number, string} -> {{:seq, regex}, sub_number, string}
    end
  end

  defp re_seq1(<<c, _rest::binary>> = string, sub_number) when c != ?| and c != ?) do
    {regex, sub_number, string} = re_repeat(string, sub_number)
    {regex2, sub_number, string} = re_seq1(string, sub_number)
    {[regex | regex2], sub_number, string}
  end

  defp re_seq1(string, sub_number), do: {[], sub_number, string}

  defp re_repeat(string, sub_number) do
    {regex, sub_number, string} = re_single(string, sub_number)
    re_repeat1(string, sub_number, regex)
  end

  defp re_repeat1(<<?*, string::binary>>, sub_number, regex) do
    re_repeat1(string, sub_number, {:kclosure, regex})
  end

  defp re_repeat1(<<?+, string::binary>>, sub_number, regex) do
    re_repeat1(string, sub_number, {:pclosure, regex})
  end

  defp re_repeat1(<<??, string::binary>>, sub_number, regex) do
    re_repeat1(string, sub_number, {:optional, regex})
  end

  defp re_repeat1(string, sub_number, regex), do: {regex, sub_number, string}

  defp re_single(<<?(, string::binary>>, sub_number) do
    sub_number = sub_number + 1

    case re_reg(string, sub_number) do
      {regex, sub_number, <<?), string::binary>>} -> {regex, sub_number, string}
      _ -> Util.parse_error({:unterminated, "("})
    end
  end

  defp re_single(<<?., string::binary>>, sub_number) do
    {{:comp_class, [?\n]}, sub_number, string}
  end

  defp re_single("[^" <> string, sub_number) do
    case re_char_class(string) do
      {regex, <<?], string::binary>>} -> {{:comp_class, regex}, sub_number, string}
      _ -> Util.parse_error({:unterminated, "["})
    end
  end

  defp re_single(<<?[, string::binary>>, sub_number) do
    case re_char_class(string) do
      {regex, <<?], string::binary>>} -> {{:char_class, regex}, sub_number, string}
      _ -> Util.parse_error({:unterminated, "["})
    end
  end

  defp re_single(<<?\\, string::binary>>, sub_number) do
    {regex, string} = re_char(?\\, string)
    {{:lit, [regex]}, sub_number, string}
  end

  defp re_single(<<c, string::binary>>, sub_number) do
    if Util.special_char(c) do
      Util.parse_error({:illegal_char, [c]})
    else
      {^c, string} = re_char(c, string)
      {{:lit, [c]}, sub_number, string}
    end
  end

  defp re_char_class(<<?], string::binary>>), do: re_char_class(string, [?]])
  defp re_char_class(string), do: re_char_class(string, [])

  defp re_char_class(<<c, string::binary>>, regex) when c != ?] do
    case re_char(c, string) do
      {char_value, <<?-, c2, string_2::binary>>} when c2 != ?] ->
        case re_char(c2, string_2) do
          {char_value_2, string_3} when char_value < char_value_2 ->
            re_char_class(string_3, [{:range, char_value, char_value_2} | regex])

          {_, string_3} ->
            Util.parse_error({:char_class, Util.string_between(<<c>> <> string, string_3)})
        end

      {c, string} ->
        re_char_class(string, [c | regex])
    end
  end

  defp re_char_class(string, regex), do: {Enum.reverse(regex), string}

  defp re_char(?\\, <<c1, c2, c3, string::binary>>)
       when c1 >= ?0 and c1 <= ?7 and c2 >= ?0 and c2 <= ?7 and c3 >= ?0 and c3 <= ?7 do
    {(c1 * 8 + c2) * 8 + c3 - 73 * ?0, string}
  end

  defp re_char(?\\, <<?x, c1, c2, string::binary>>) when Util.is_hex(c1) and Util.is_hex(c2) do
    {List.to_integer([c1, c2], 16), string}
  end

  defp re_char(?\\, <<?x, ?{, string::binary>>), do: re_hex(string, [])

  defp re_char(?\\, <<?x, _rest::binary>>) do
    Util.parse_error({:illegal_char, "\\x"})
  end

  defp re_char(?\\, <<c, string::binary>>), do: {Util.escape_char(c), string}
  defp re_char(?\\, ""), do: Util.parse_error({:unterminated, "\\"})
  defp re_char(c, string), do: {c, string}

  defp re_hex(<<c, string::binary>>, list_of_hex) when Util.is_hex(c),
    do: re_hex(string, [c | list_of_hex])

  defp re_hex(<<?}, string::binary>>, list_of_hex) do
    list_of_hex = Enum.reverse(list_of_hex)

    case List.to_integer(list_of_hex, 16) do
      c when c <= 0x10FFFF ->
        {c, string}

      _ ->
        Util.parse_error({:illegal_char, <<?\\, ?x, ?{>> <> List.to_string(list_of_hex) <> "}"})
    end
  end

  defp re_hex(_, _), do: Util.parse_error({:unterminated, "\\x{"})
end

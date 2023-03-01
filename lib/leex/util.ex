defmodule Leex.Util do
  alias Leex.Util.OrdSet

  defguard is_hex(c)
           when (c >= ?0 and c <= ?9) or
                  (c >= ?A and c <= ?F) or
                  (c >= ?a and c <= ?f)

  def apply_file_names(state, file_name, opts) do
    dir = Path.dirname(file_name)
    base = Path.basename(file_name, ".xex")
    xfile = Path.join(dir, base <> ".xex")
    efile = base <> ".ex"
    gfile = base <> ".dot"
    module = String.to_atom(base)
    ifile = Keyword.get(opts, :include_file, "leex.inc.ex")
    ofile = Keyword.get(opts, :scanner_file)

    state = %{state | xfile: xfile, ifile: ifile, opts: opts, module: module}

    if ofile == nil do
      %{state | efile: Path.join(dir, efile), gfile: Path.join(dir, gfile)}
    else
      %{state | efile: ofile, gfile: Path.join(Path.dirname(ofile), gfile)}
    end
  end

  def next_line(stream, line_number, state) do
    case IO.gets(stream, :leex) do
      :eof ->
        {:eof, line_number}

      {:error, _reason} ->
        add_error({line_number + 1, :leex, :cannot_parse}, state)

      data ->
        case String.trim(data) do
          "" -> next_line(stream, line_number + 1, state)
          "#" <> _rest -> next_line(stream, line_number + 1, state)
          _ -> {:ok, data, line_number + 1}
        end
    end
  end

  def add_error(error, state) do
    add_error(state.xfile, error, state)
  end

  def add_error(file, error, state) do
    throw(%{state | errors: [{file, error} | state.errors]})
  end

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

  def count_lines(file, line_number, state) do
    case IO.gets(file, :leex) do
      :eof -> line_number
      {:error, _} -> add_error({line_number + 1, :leex, :cannot_parse}, state)
      _line -> count_lines(file, line_number + 1, state)
    end
  end

  def epsilon_trans(firsts), do: Enum.map(firsts, fn f -> {:epsilon, f} end)

  def pack_cc(char_class) do
    List.foldl(char_class, OrdSet.new(), fn
      {:range, cf, cl}, set -> OrdSet.put(set, {cf, cl})
      c, set -> OrdSet.put(set, {c, c})
    end)
    |> Enum.sort()
    |> pack_crs()
  end

  def comp_class(comp_class) do
    pack_cc(comp_class)
    |> comp_crs(0)
  end

  def eclosure(states, nfa), do: eclosure(states, nfa, OrdSet.new())

  def move(states, cr, nfa) do
    for n <- states do
      for {crs, state} <- elem(nfa, n - 1).edges, crs != :epsilon, in_crs(cr, crs), do: state
    end
    |> List.flatten()
  end

  def werror(state) do
    state.warnings != [] and
      Enum.member?(state.opts, :warnings_as_errors)
  end

  def report_errors(state) do
    if Keyword.has_key?(state.opts, :report_errors) do
      state.errors
      |> Enum.sort()
      |> Enum.each(fn
        {file, {:none, module, error}} ->
          :io.fwrite(
            "~ts: ~ts\n",
            [file, apply(module, :format_error, [error])]
          )

        {file, {line, module, error}} ->
          :io.fwrite(
            "~ts:~w: ~ts\n",
            [file, line, apply(module, :format_error, [error])]
          )
      end)
    end
  end

  def report_warnings(state) do
    werror = Keyword.has_key?(state.opts, :warnings_as_errors)

    prefix =
      if werror do
        ""
      else
        "Warning: "
      end

    report_werror = werror and Keyword.has_key?(state.opts, :report_errors)
    should_report = Keyword.has_key?(state.opts, :report_warnings) or report_werror

    if should_report do
      state.warnings
      |> Enum.sort()
      |> Enum.each(fn
        {file, {:none, module, warning}} ->
          :io.fwrite(
            "~ts: ~s~ts\n",
            [file, prefix, apply(module, :format_error, [warning])]
          )

        {file, {line, module, warning}} ->
          :io.fwrite(
            "~ts:~w: ~s~ts\n",
            [file, line, prefix, apply(module, :format_error, [warning])]
          )
      end)
    end
  end

  def pack_errors([{file, _} | _] = errors) do
    [{file, errors |> Enum.sort() |> Enum.flat_map(fn {_, error} -> [error] end)}]
  end

  def pack_errors([]), do: []

  def do_error_return(state, errors, warnings) do
    if Keyword.has_key?(state.opts, :return_errors) do
      {:error, errors, warnings}
    else
      :error
    end
  end

  defp in_crs({c1, c2}, [{c3, c4} | _crs]) when c1 >= c3 and c2 <= c4, do: true
  defp in_crs(cr, [cr | _crs]), do: true
  defp in_crs(cr, [_ | crs]), do: in_crs(cr, crs)
  defp in_crs(_cr, []), do: false

  defp eclosure([state | states], nfa, eclosure) do

    %Leex.NfaState{edges: edges} = elem(nfa, state - 1)

    eclosure(
      for({:epsilon, n} <- edges, !OrdSet.member?(eclosure, n), do: n) ++ states,
      nfa,
      OrdSet.put(eclosure, state)
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

  def string_take(string, chars, true) do
    string_take(string, chars, true, {"", string})
  end

  defp string_take("", _chars, true, acc), do: acc

  defp string_take(<<c, string::binary>>, chars, true, {part_1, _} = acc) do
    if c in chars do
      acc
    else
      string_take(string, chars, true, {part_1 <> <<c>>, string})
    end
  end
end

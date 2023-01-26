defmodule Leex.Util do
  defguard is_hex(c)
           when (c >= ?0 and c <= ?9) or
                  (c >= ?A and c <= ?F) or
                  (c >= ?a and c <= ?f)

  def apply_file_names(state, file_name, opts) do
    dir = Path.dirname(file_name)
    base = Path.basename(file_name, ".xrl")
    xfile = Path.join(dir, base <> ".xrl")
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
    raise %{state | errors: [{file, error} | state.errors]}
  end

  def parse_error(error) do
    raise {:parse_error, error}
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
end

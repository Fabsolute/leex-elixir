defmodule Leex do
  alias Leex.Util
  alias Leex.Util.Parser

  # xex file
  defstruct xfile: [],
            # ex file
            efile: [],
            # include file ??
            ifile: [],
            # graph file
            gfile: [],
            # module name
            module: nil,
            # options
            opts: [],
            errors: [],
            warnings: []

  def file(file_name), do: file(file_name, [])

  def file(file_name, opts) when is_list(opts) do
    state = %Leex{} |> Util.apply_file_names(file_name, opts) |> parse_file()
  end

  def file(file_name, opt), do: file(file_name, [opt])

  defp parse_file(state) do
    try do
      stream = File.stream!(state.xfile)
      {line, state} = Parser.parse_head(stream, state)
      {line, defs, state} = Parser.parse_defs(stream, line, state)
      {} = Parser.parse_rules(stream, line, defs, state)
    catch
      error -> Util.add_error({:none, :leex, {:file_error, error}}, state)
    end
  end
end

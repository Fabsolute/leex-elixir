defmodule Leex do
  alias Leex.Util
  alias Leex.Util.Parser
  alias Leex.Util.DFA
  alias Leex.Util.Writer

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

  def init(name, fun) do
    :dbg.start()
    :dbg.tracer()
    :dbg.p(:all, :c)
    :dbg.tpl(name, fun, [])
  end

  def file(file_name), do: file(file_name, [])

  def file(file_name, opts) when is_list(opts) do
    {regex_actions, actions, code, state} =
      %Leex{}
      |> Util.apply_file_names(file_name, opts)
      |> parse_file()

    {dfa, dfa_first} = DFA.make_dfa(regex_actions)

    unless Util.werror(state) do
      # [state: state, dfa: dfa, dfa_first: dfa_first, actions: actions, code: code]|>IO.inspect()
      Writer.out_file(state, dfa, dfa_first, actions, code)
    else
      state
    end
    |> leex_ret()
  end

  def file(file_name, opt), do: file(file_name, [opt])

  defp leex_ret(state) do
    Util.report_errors(state)
    Util.report_warnings(state)
    errors = Util.pack_errors(state.errors)
    warnings = Util.pack_errors(state.warnings)

    cond do
      Util.werror(state) ->
        Util.do_error_return(state, errors, warnings)

      errors == [] ->
        if Keyword.has_key?(state.opts, :return_warnings) do
          {:ok, state.efile, warnings}
        else
          {:ok, state.efile}
        end

      true ->
        Util.do_error_return(state, errors, warnings)
    end
  end

  defp parse_file(state) do
    try do
      stream = File.open!(state.xfile)

      try do
        {line, state} = Parser.parse_head(stream, state)
        {line, defs, state} = Parser.parse_defs(stream, line, state)

        {line, regex_actions, actions, state} = Parser.parse_rules(stream, line, defs, state)
        {code, state} = Parser.parse_code(stream, line, state)
        {regex_actions, actions, code, state}
      after
        File.close(stream)
      end
    catch
      error -> Util.add_error({:none, :leex, {:file_error, error}}, state)
    end
  end
end

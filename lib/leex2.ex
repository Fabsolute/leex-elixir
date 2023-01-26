defmodule Leex2 do
  @leex_inc 'leexinc.hrl'
  @defs_head '@Definitions'
  @rule_head '@Rules'
  @code_head '@Code'

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
            # encoding of xex file
            encoding: :none,
            errors: [],
            warnings: []

  def compile(input, output, %Leex.Options{
        warning: warn_level,
        verbose: verbose,
        includes: includes,
        specific: specific
      }) do
    input = assure_extension(shorten_filename(input), '.xex')
    output = assure_extension(shorten_filename(output), '.ex')
    # todo
    include_file = :lists.sublist(includes, 1)
    # todo
    w_error = Keyword.get(specific, :warnings_as_errors, false)
    deterministic = Keyword.get(specific, :deterministic, false)

    opts = [
      scannerfile: output,
      includefile: include_file,
      verbose: verbose,
      report_errors: true,
      report_warnings: warn_level > 0,
      warnings_as_errors: w_error,
      deterministic: deterministic
    ]

    case file(input, opts) do
      {:ok, _} ->
        :ok

      :error ->
        :error
    end
  end

  def file(file) do
    file(file, [])
  end

  def file(file, opts) when is_list(opts) do
    if is_filename(file) == false do
      :erlang.error(:badarg, [file, opts])
    end

    env_opts = env_default_opts() |> select_recognized_opts()
    opts1 = opts ++ env_opts

    opts =
      case options(opts1) do
        :badarg ->
          :erlang.error(:badarg, [file, opts])

        options ->
          options
      end

    st = %__MODULE__{}
    st = filenames(file, opts, st)

    try do
      {:ok, reas, actions, code, st1} = parse_file(st)
      {dfa, df} = make_dfa(reas, st1)

      unless werror(st1) do
        st2 = out_file(st1, dfa, df, actions, code)

        if :lists.member(:dfa_graph, st2.opts) do
          out_dfa_graph(st2, dfa, df)
        else
          st2
        end
      else
        st1
      end
    catch
      %__MODULE__{} = st3 ->
        st3
    end
    |> leex_ret()
  end

  def file(file, opt) do
    file(file, [opt])
  end

  defguardp is_hex(c)
            when (c >= ?0 and c <= ?9) or
                   (c >= ?A and c <= ?F) or
                   (c >= ?a and c <= ?f)

  defp is_filename(file) do
    try do
      :filename.flatten(file)
    catch
      :error -> false
    end
  end

  defp assure_extension(file, ext) do
    :lists.concat([strip_extension(file, ext), ext])
  end

  defp strip_extension(file, ext) do
    case :filename.extension(file) do
      ^ext -> :filename.rootname(file)
      _other -> file
    end
  end

  defp shorten_filename(name) do
    {:ok, cwd} = :file.get_cwd()

    case :string.prefix(name, cwd) do
      :nomatch ->
        name

      rest ->
        case :unicode.characters_to_list(rest) do
          '/' ++ n -> n
          n -> n
        end
    end
  end

  defp env_default_opts do
    key = 'EX_COMPILER_OPTIONS'

    case :os.getenv(key) do
      false ->
        []

      str when is_list(str) ->
        case :erl_scan.string(str) do
          {:ok, tokens, _} ->
            dot = {:dot, :erl_anno.new(1)}

            case :erl_parse.parse_term(tokens ++ [dot]) do
              {:ok, list} when is_list(list) ->
                list

              {:ok, term} ->
                [term]

              {:error, _reason} ->
                :io.format('Ignoring bad term in ~s\n', [key])
                []
            end

          {:error, {_, _, _reason}, _} ->
            :io.format('Ignoring bad term in ~s\n', [key])
            []
        end
    end
  end

  defp select_recognized_opts(options) do
    all_options = all_options()

    preprocess_options(options)
    |> Enum.filter(fn {name, _} -> :lists.member(name, all_options) end)
  end

  defp options(options) do
    all_options = all_options()
    options = preprocess_options(options)

    case check_options(options, all_options, []) do
      :badarg ->
        :badarg

      option_values ->
        all_option_values =
          all_options
          |> Enum.map(fn option ->
            case :lists.keyfind(option, 1, option_values) do
              false ->
                {option, default_option(option)}

              option_value ->
                option_value
            end
          end)

        :lists.foldr(
          fn
            {_, false}, l -> l
            {option, true}, l -> [option | l]
            option_value, l -> [option_value | l]
          end,
          [],
          all_option_values
        )
    end
  end

  defp filenames(file, opts, st) do
    dir = :filename.dirname(file)
    base = :filename.basename(file, '.xex')
    xfile = :filename.join(dir, base ++ '.xex')
    efile = base ++ '.ex'
    gfile = base ++ '.dot'
    module = :erlang.list_to_atom(base)
    st = %{st | xfile: xfile, opts: opts, module: module}
    {:includefile, ifile} = :lists.keyfind(:includefile, 1, opts)
    ifile = inc_file_name(ifile)
    {:scannerfile, ofile} = :lists.keyfind(:scannerfile, 1, opts)

    if ofile == [] do
      %{st | efile: :filename.join(dir, efile), ifile: ifile, gfile: :filename.join(dir, gfile)}
    else
      d = :filename.dirname(ofile)
      %{st | efile: ofile, ifile: ifile, gfile: :filename.join(d, gfile)}
    end
  end

  defp parse_file(st) do
    case :file.open(st.xfile, [:read]) do
      {:ok, xfile} ->
        st = %{st | encoding: :epp.set_encoding(xfile)}

        try do
          verbose_print(st, 'Parsing file ~ts, ', [st.xfile])
          {:ok, line, st} = parse_head(xfile, st)
          {:ok, line, macs, st} = parse_defs(xfile, line, st)
          {:ok, line, reas, actions, st} = parse_rules(xfile, line, macs, st)
          {:ok, code, st} = parse_code(xfile, line, st)
          verbose_print(st, 'contained ~w rules.~n', [length(reas)])
          {:ok, reas, actions, code, st}
        after
          :ok = :file.close(xfile)
        end

      {:error, error} ->
        add_error({:none, :leex, {:file_error, error}}, st)
    end
  end

  defp make_dfa(reas, st) do
    {nfa, nf} = build_combined_nfa(reas)
    verbose_print(st, 'NFA contains ~w states, ', [tuple_size(nfa)])
    {dfa, df} = build_dfa(nfa, nf)
    verbose_print(st, 'DFA contains ~w states, ', [length(dfa)])
    {dfa, df} = minimise_dfa(dfa, df)
    verbose_print(st, 'minimised to ~w states.~n', [length(dfa)])
    {dfa, df}
  end

  defp werror(st) do
    st.warnings != [] and
      :lists.member(:warnings_as_errors, st.opts)
  end

  defp out_file(st, dfa, df, actions, code) do
    verbose_print(st, 'Writing file ~ts, ', [st.efile])

    case open_inc_file(st) do
      {:ok, ifile} ->
        try do
          case :file.open(st.efile, [:write]) do
            {:ok, ofile} ->
              set_encoding(st, ofile)

              try do
                output_encoding_comment(ofile, st)
                deterministic = :proplists.get_bool(:deterministic, st.opts)
                output_file_directive(ofile, st.ifile, deterministic, 0)
                out_file(ifile, ofile, st, dfa, df, actions, code, 1)
                verbose_print(st, 'ok~n', [])
                st
              after
                :ok = :file.close(ofile)
              end

            {:error, error} ->
              verbose_print(st, 'error~n', [])
              add_error({:none, :leex, {:file_error, error}}, st)
          end
        after
          :ok = :file.close(ifile)
        end

      {{:error, error}, ifile} ->
        add_error(ifile, {:none, :leex, {:file_error, error}}, st)
    end
  end

  defp out_dfa_graph(st, dfa, df) do
    verbose_print(st, 'Writing DFA to file ~ts, ', [st.gfile])

    case :file.open(st.gfile, [:write]) do
      {:ok, gfile} ->
        try do
          set_encoding(st, gfile)
          out_dfa_states(gfile, dfa, df)
          out_dfa_edges(gfile, dfa)
          verbose_print(st, 'ok~n', [])
          st
        after
          :ok = :file.close(gfile)
        end

      {:error, error} ->
        verbose_print(st, 'error~n', [])
        add_error({:none, :leex, {:file_error, error}}, st)
    end
  end

  defp leex_ret(st) do
    report_errors(st)
    report_warnings(st)
    es = pack_errors(st.errors)
    ws = pack_warnings(st.warnings)
    werror = werror(st)

    if werror do
      do_error_return(st, es, ws)
    else
      if es == [] do
        if :lists.member(:return_warnings, st.opts) do
          {:ok, st.efile, ws}
        else
          {:ok, st.efile}
        end
      else
        do_error_return(st, es, ws)
      end
    end
  end

  defp all_options() do
    [
      :dfa_graph,
      :includefile,
      :report_errors,
      :report_warnings,
      :return_errors,
      :return_warnings,
      :scannerfile,
      :verbose,
      :warnings_as_errors,
      :deterministic
    ]
  end

  defp preprocess_options(options) do
    :lists.foldr(&preproc_opt/2, [], options)
  end

  defp check_options([{option, file_name} | options], all_options, l)
       when option == :includefile or option == :scannerfile do
    case is_filename(file_name) do
      false ->
        :badarg

      filename ->
        check_options(options, all_options, [{option, filename} | l])
    end
  end

  defp check_options([{option, boolean} | options], all_options, l) when is_boolean(boolean) do
    if :lists.member(option, all_options) do
      check_options(options, all_options, [{option, boolean} | l])
    else
      :badarg
    end
  end

  defp check_options([], _all_options, l), do: l
  defp check_options(_options, _, _l), do: :badarg

  defp default_option(:dfa_graph), do: false
  defp default_option(:includefile), do: []
  defp default_option(:report_errors), do: true
  defp default_option(:report_warnings), do: true
  defp default_option(:return_errors), do: false
  defp default_option(:return_warnings), do: false
  defp default_option(:scannerfile), do: []
  defp default_option(:verbose), do: false
  defp default_option(:warnings_as_errors), do: false
  defp default_option(:deterministic), do: false

  defp inc_file_name([]) do
    incdir = :filename.join(:code.lib_dir(:parsetools), 'include')
    :filename.join(incdir, @leex_inc)
  end

  defp inc_file_name(filename), do: filename

  defp verbose_print(st, format, args) do
    when_opt(fn -> :io.fwrite(format, args) end, :verbose, st.opts)
  end

  defp parse_head(ifile, st), do: {:ok, nextline(ifile, 0, st), st}

  defp parse_defs(ifile, {:ok, @defs_head ++ rest, l}, st) do
    st1 = warn_ignored_chars(l, rest, st)
    parse_defs(ifile, nextline(ifile, l, st), [], st1)
  end

  defp parse_defs(_, {:ok, _, l}, st) do
    add_error({l, :leex, :missing_defs}, st)
  end

  defp parse_defs(_, {:eof, l}, st) do
    add_error({l, :leex, :missing_defs}, st)
  end

  defp parse_defs(ifile, {:ok, chars, l} = line, ms, st) do
    ms2 = '^[ \t]*([A-Z_][A-Za-z0-9_]*)[ \t]*=[ \t]*([^ \t\r\n]*)[ \t\r\n]*\$'

    case :re.run(chars, ms2, [{:capture, :all_but_first, :list}, :unicode]) do
      {:match, [name, def]} ->
        parse_defs(ifile, nextline(ifile, l, st), [{name, def} | ms], st)

      _ ->
        {:ok, line, ms, st}
    end
  end

  defp parse_defs(_, line, ms, st) do
    {:ok, line, ms, st}
  end

  defp parse_rules(ifile, {:ok, @rule_head ++ rest, l}, ms, st) do
    st1 = warn_ignored_chars(l, rest, st)
    parse_rules(ifile, nextline(ifile, l, st), ms, [], [], 0, st1)
  end

  defp parse_rules(_, {:ok, _, l}, _, st) do
    add_error({l, :leex, :missing_rules}, st)
  end

  defp parse_rules(_, {:eof, l}, _, st) do
    add_error({l, :leex, :missing_rules}, st)
  end

  defp parse_rules(ifile, next_line, ms, reas, as, n, st) do
    case next_line do
      {:ok, @code_head ++ _rest, _} ->
        parse_rules_end(ifile, next_line, reas, as, st)

      {:ok, chars, l0} ->
        case collect_rule(ifile, chars, l0) do
          {:ok, re, atoks, l1} ->
            {:ok, rea, a, st1} = parse_rule(re, l0, atoks, ms, n, st)
            parse_rules(ifile, nextline(ifile, l1, st), ms, [rea | reas], [a | as], n + 1, st1)

          {:error, e} ->
            add_error(e, st)
        end

      {:eof, _} ->
        parse_rules_end(ifile, next_line, reas, as, st)
    end
  end

  defp parse_code(ifile, {:ok, @code_head ++ rest, code_l}, st) do
    st1 = warn_ignored_chars(code_l, rest, st)
    {:ok, code_pos} = :file.position(ifile, :cur)
    end_code_line = count_lines(ifile, code_l, st)
    n_code_lines = end_code_line - code_l
    {:ok, {code_l, code_pos, n_code_lines}, st1}
  end

  defp parse_code(_, {:ok, _, l}, st) do
    add_error({l, :leex, :missing_code}, st)
  end

  defp parse_code(_, {:eof, l}, st) do
    add_error({l, :leex, :missing_code}, st)
  end

  defp add_error(e, st), do: add_error(st.xfile, e, st)

  defp add_error(file, error, st), do: :erlang.throw(%{st | errors: [{file, error} | st.errors]})

  defp add_warning(line, w, st) do
    %{st | warnings: [{st.xfile, {line, :leex, w}} | st.warnings]}
  end

  defp build_combined_nfa(reas) do
    {nfa0, firsts, free} = build_nfa_list(reas, [], [], 1)
    f = %Leex.NfaState{no: free, edges: epsilon_trans(firsts)}
    {:erlang.list_to_tuple(Enum.sort_by([f | nfa0], & &1.no)), free}
  end

  defp build_nfa_list([{re, action} | reas], nfa0, firsts, free0) do
    {nfa1, free1, first} = build_nfa(re, free0, action)
    build_nfa_list(reas, nfa1 ++ nfa0, [first | firsts], free1)
  end

  defp build_nfa_list([], nfa, firsts, free) do
    {nfa, :lists.reverse(firsts), free}
  end

  defp build_dfa(nfa, nf) do
    d = %Leex.DfaState{no: 0, nfa: eclosure([nf], nfa)}
    {build_dfa([d], 1, [], nfa), 0}
  end

  defp minimise_dfa(dfa0, df0) do
    case min_dfa(dfa0) do
      {dfa1, []} ->
        {dfa2, rs} = pack_dfa(dfa1)
        {min_update(dfa2, rs), min_use(df0, rs)}

      {dfa1, rs} ->
        minimise_dfa(min_update(dfa1, rs), min_use(df0, rs))
    end
  end

  defp open_inc_file(state) do
    ifile = state.ifile

    case :file.open(ifile, [:read]) do
      {:ok, f} ->
        _ = :epp.set_encoding(f)
        {:ok, f}

      error ->
        {error, ifile}
    end
  end

  defp set_encoding(%__MODULE__{encoding: :none}, file) do
    :ok = :io.setopts(file, [{:encoding, :epp.default_encoding()}])
  end

  defp set_encoding(%__MODULE__{encoding: e}, file) do
    :ok = :io.setopts(file, [{:encoding, e}])
  end

  defp output_encoding_comment(_file, %__MODULE__{encoding: :none}), do: :ok

  defp output_encoding_comment(file, %__MODULE__{encoding: encoding}) do
    :io.fwrite(file, "%% ~s\n", [:epp.encoding_to_string(encoding)])
  end

  defp output_file_directive(file, filename, deterministic, line) do
    :io.fwrite(file, "-file(~ts, ~w).\n", [
      format_filename(filename, file, deterministic),
      line
    ])
  end

  defp out_dfa_states(file, dfa, df) do
    Enum.each(dfa, fn s -> out_dfa_state(file, df, s) end)
    :io.fwrite(file, '~n', [])
  end

  defp out_dfa_edges(file, dfa) do
    Enum.each(dfa, fn %Leex.DfaState{no: s, trans: trans} ->
      pt = pack_trans(trans)
      tdict = :lists.foldl(fn {cr, t}, d -> :orddict.append(t, cr, d) end, :orddict.new(), pt)

      tdict
      |> :orddict.fetch_keys()
      |> :lists.sort()
      |> Enum.each(fn t ->
        crs = :orddict.fetch(t, tdict)
        edgelab = dfa_edgelabel(crs, file)
        :io.fwrite(file, '  ~b -> ~b [label=\"~ts\"];~n', [s, t, edgelab])
      end)
    end)
  end

  defp report_errors(st) do
    when_opt(
      fn ->
        st.errors
        |> :lists.sort()
        |> Enum.each(fn
          {file, {:none, mod, e}} ->
            :io.fwrite(
              '~ts: ~ts\n',
              [file, apply(mod, :format_error, [e])]
            )

          {file, {line, mod, e}} ->
            :io.fwrite(
              '~ts:~w: ~ts\n',
              [file, line, apply(mod, :format_error, [e])]
            )
        end)
      end,
      :report_errors,
      st.opts
    )
  end

  defp report_warnings(st) do
    werror = :lists.member(:warnings_as_errors, st.opts)

    prefix =
      unless werror do
        'Warning: '
      else
        ''
      end

    report_werror = werror and :lists.member(:report_errors, st.opts)
    should_report = :lists.member(:report_warnings, st.opts) or report_werror

    when_bool(
      fn ->
        st.warnings
        |> :lists.sort()
        |> Enum.each(fn
          {file, {:none, mod, w}} ->
            :io.fwrite(
              '~ts: ~s~ts\n',
              [file, prefix, apply(mod, :format_error, [w])]
            )

          {file, {line, mod, w}} ->
            :io.fwrite(
              '~ts:~w: ~s~ts\n',
              [file, line, prefix, apply(mod, :format_error, [w])]
            )
        end)
      end,
      should_report
    )
  end

  defp pack_errors([{file, _} | _] = es) do
    [{file, :lists.flatmap(fn {_, e} -> [e] end, :lists.sort(es))}]
  end

  defp pack_errors([]), do: []

  defp pack_warnings([{file, _} | _] = ws) do
    [{file, :lists.flatmap(fn {_, w} -> [w] end, :lists.sort(ws))}]
  end

  defp pack_warnings([]), do: []

  defp do_error_return(st, es, ws) do
    if :lists.member(:return_errors, st.opts) do
      {:error, es, ws}
    else
      :error
    end
  end

  defp preproc_opt(:return, os) do
    [{:return_errors, true}, {:return_warnings, true} | os]
  end

  defp preproc_opt(:report, os) do
    [{:report_errors, true}, {:report_warnings, true} | os]
  end

  defp preproc_opt({:return, t}, os) do
    [{:return_errors, t}, {:return_warnings, t} | os]
  end

  defp preproc_opt({:report, t}, os) do
    [{:report_errors, t}, {:report_warnings, t} | os]
  end

  defp preproc_opt(option, os) do
    [
      try do
        atom_option(option)
      catch
        # todo error:_
        _ -> option
      end
      | os
    ]
  end

  defp when_opt(fun, opt, opts) do
    if :lists.member(opt, opts) do
      fun.()
    else
      :ok
    end
  end

  defp nextline(ifile, l, st) do
    case :io.get_line(ifile, :leex) do
      :eof ->
        {:eof, l}

      {:error, _} ->
        add_error({l + 1, :leex, :cannot_parse}, st)

      chars ->
        case :string.take(chars, ' \t\n') do
          {_, [?% | _rest]} -> nextline(ifile, l + 1, st)
          {_, []} -> nextline(ifile, l + 1, st)
          _other -> {:ok, chars, l + 1}
        end
    end
  end

  defp warn_ignored_chars(line, s, st) do
    case non_white(s) do
      [] -> st
      _ -> add_warning(line, :ignored_characters, st)
    end
  end

  defp parse_rules_end(_, {:ok, _, l}, [], [], st) do
    add_error({l, :leex, :empty_rules}, st)
  end

  defp parse_rules_end(_, {:eof, l}, [], [], st) do
    add_error({l, :leex, :empty_rules}, st)
  end

  defp parse_rules_end(_, next_line, reas, as, st) do
    {:ok, next_line, :lists.reverse(reas), :lists.reverse(as), st}
  end

  defp collect_rule(ifile, chars, l0) do
    {reg_exp, rest} = :string.take(chars, ' \t\r\n', true)

    case collect_action(ifile, rest, l0, []) do
      {:ok, [{:":", _} | toks], l1} -> {:ok, reg_exp, toks, l1}
      {:ok, _, _} -> {:error, {l0, :leex, :bad_rule}}
      {:eof, l1} -> {:error, {l1, :leex, :bad_rule}}
      {:error, e, _} -> {:error, e}
    end
  end

  defp parse_rule(s, line, [{:dot, _}], ms, n, st) do
    case parse_rule_regexp(s, ms, st) do
      {:ok, r} ->
        {:ok, {r, n}, {n, :empty_action}, st}

      {:error, e} ->
        add_error({line, :leex, e}, st)
    end
  end

  defp parse_rule(s, line, atoks, ms, n, st) do
    case parse_rule_regexp(s, ms, st) do
      {:ok, r} ->
        token_chars = var_used('TokenChars', atoks)
        token_len = var_used('TokenLen', atoks)
        token_line = var_used('TokenLine', atoks)
        {:ok, {r, n}, {n, atoks, token_chars, token_len, token_line}, st}

      {:error, e} ->
        add_error({line, :leex, e}, st)
    end
  end

  defp count_lines(file, n, st) do
    case :io.get_line(file, :leex) do
      :eof -> n
      {:error, _} -> add_error({n + 1, :leex, :cannot_parse}, st)
      _line -> count_lines(file, n + 1, st)
    end
  end

  defp epsilon_trans(firsts) do
    firsts |> Enum.map(fn first -> {:epsilon, first} end)
  end

  defp build_nfa(re, n0, action) do
    {nfa, n1, e} = build_nfa(re, n0 + 1, n0, [])
    {[%Leex.NfaState{no: e, accept: {:accept, action}} | nfa], n1, n0}
  end

  defp eclosure(sts, nfa), do: eclosure(sts, nfa, [])

  defp eclosure([st | sts], nfa, ec) do
    %Leex.NfaState{edges: es} = :erlang.element(st, nfa)

    lst =
      es
      |> Enum.filter(fn
        {:epsilon, n} ->
          not :ordsets.is_element(n, ec)

        _ ->
          false
      end)
      |> Enum.map(fn {:epsilon, n} -> n end)

    eclosure(lst ++ sts, nfa, :ordsets.add_element(st, ec))
  end

  defp eclosure([], _, ec), do: ec

  defp build_dfa([u | us0], n0, ms, nfa) do
    {ts, us1, n1} = build_dfa(u.nfa, us0, n0, [], [u | ms], nfa)
    m = %{u | trans: ts, accept: accept(u.nfa, nfa)}
    build_dfa(us1, n1, [m | ms], nfa)
  end

  defp build_dfa([], _, ms, _), do: ms

  defp min_dfa(dfa), do: min_dfa(dfa, [], [])

  defp min_dfa([d | dfa0], rs0, mdfa) do
    {dfa1, rs1} = min_delete(dfa0, d.trans, d.accept, d.no, rs0, [])
    min_dfa(dfa1, rs1, [d | mdfa])
  end

  defp min_dfa([], rs, mdfa), do: {mdfa, rs}

  defp pack_dfa(dfa) do
    pack_dfa(dfa, 0, [], [])
  end

  defp pack_dfa([d | dfa], new_n, rs, pdfa) do
    pack_dfa(dfa, new_n + 1, [{d.no, new_n} | rs], [%{d | no: new_n} | pdfa])
  end

  defp pack_dfa([], _, rs, pdfa) do
    {pdfa, rs}
  end

  defp min_update(dfa, rs) do
    Enum.map(dfa, fn d -> %{d | trans: min_update_trans(d.trans, rs)} end)
  end

  defp min_update_trans(tr, rs) do
    Enum.map(tr, fn {c, s} -> {c, min_use(s, rs)} end)
  end

  defp min_use(old, reds) do
    case Enum.find(reds, fn {o, _} -> o == old end) do
      nil -> old
      {_, new} -> new
    end
  end

  defp format_filename(filename, file, deterministic) do
    filename =
      if deterministic do
        :filename.basename(:filename.flatten(filename))
      else
        :filename.flatten(filename)
      end

    case enc(file) do
      :unicode -> :io_lib.write_string(filename)
      :latin1 -> :io_lib.write_string_as_latin1(filename)
    end
  end

  defp out_module(file, st) do
    :io.fwrite(file, '-module(~w).\n', [st.module])
  end

  defp out_erlang_code(file, st, code, l) do
    {code_l, code_pos, _n_code_lines} = code
    deterministic = :proplists.get_bool(:deterministic, st.opts)
    output_file_directive(file, st.xfile, deterministic, code_l)
    {:ok, xfile} = :file.open(st.xfile, [:read])

    try do
      set_encoding(st, xfile)
      {:ok, _} = :file.position(xfile, code_pos)
      :ok = file_copy(xfile, file)
    after
      :ok = :file.close(xfile)
    end

    :io.nl(file)
    output_file_directive(file, st.ifile, deterministic, l)
  end

  defp out_dfa(file, st, dfa, code, df, l) do
    {_code_l, _code_pos, n_code_lines} = code
    deterministic = :proplists.get_bool(:deterministic, st.opts)
    output_file_directive(file, st.efile, deterministic, l + (n_code_lines - 1) + 3)
    :io.fwrite(file, 'yystate() -> ~w.~n~n', [df])
    Enum.each(dfa, &out_trans/2)
    :io.fwrite(file, 'yystate(S, Ics, Line, Tlen, Action, Alen) ->~n', [])
    :io.fwrite(file, '    {Action,Alen,Tlen,Ics,Line,S}.~n', [])
  end

  defp out_actions(file, xrl_file, deterministic, as) do
    as = prep_out_actions(as)
    Enum.each(as, fn a -> out_action(file, a) end)
    :io.fwrite(file, 'yyaction(_, _, _, _) -> error.~n', [])
    Enum.each(as, fn a -> out_action_code(file, xrl_file, deterministic, a) end)
  end

  defp out_dfa_state(file, df, %Leex.DfaState{no: df, accept: {:accept, _}}) do
    IO.write(file, '  #{df} [shape=doublecircle color=green];\n')
  end

  defp out_dfa_state(file, df, %Leex.DfaState{no: df, accept: :noaccept}) do
    IO.write(file, '  #{df} [shape=circle color=green];\n')
  end

  defp out_dfa_state(file, _, %Leex.DfaState{no: s, accept: {:accept, _}}) do
    IO.write(file, '  #{s} [shape=doublecircle];\n')
  end

  defp out_dfa_state(file, _, %Leex.DfaState{no: s, accept: :noaccept}) do
    IO.write(file, '  #{s} [shape=circle];\n')
  end

  defp pack_trans(trs), do: pack_trans(trs, [])

  defp pack_trans([{{c, c2}, s} | trs], pt) when c == c2 do
    if :lists.member({c, s}, pt) do
      pack_trans(trs, pt)
    else
      pack_trans(trs, [{c, s} | pt])
    end
  end

  defp pack_trans([{{cf, ?\n}, s} | trs], pt) do
    pack_trans([{{cf, ?\n - 1}, s} | trs], [{?\n, s} | pt])
  end

  defp pack_trans([{{?\n, cl}, s} | trs], pt) do
    pack_trans([{{?\n + 1, cl}, s} | trs], [{?\n, s} | pt])
  end

  defp pack_trans([{{cf, cl}, s} | trs], pt) when cf < ?\n or cl > ?\n do
    pack_trans([{{cf, ?\n - 1}, s}, {{?\n + 1, cl}, s} | trs], [{?\n, s} | pt])
  end

  defp pack_trans([{{cf, cl}, s} | trs], pt) when cl == cf + 1 do
    pack_trans(trs, [{cf, s}, {cl, s} | pt])
  end

  defp pack_trans([tr | trs], pt) do
    pack_trans(trs, pt ++ [tr])
  end

  defp pack_trans([], pt), do: pt

  defp dfa_edgelabel([c], file) when is_integer(c), do: quote1(c, file)

  defp dfa_edgelabel(cranges, file) do
    '[' ++
      :lists.map(
        fn
          {a, b} -> [quote1(a, file), '-', quote1(b, file)]
          c -> [quote1(c, file)]
        end,
        cranges
      ) ++ ']'
  end

  defp when_bool(fun, bool) do
    if bool do
      fun.()
    else
      :ok
    end
  end

  defp atom_option(:dfa_graph), do: {:dfa_graph, true}
  defp atom_option(:report_errors), do: {:report_errors, true}
  defp atom_option(:report_warnings), do: {:report_warnings, true}
  defp atom_option(:warnings_as_errors), do: {:warnings_as_errors, true}
  defp atom_option(:return_errors), do: {:return_errors, true}
  defp atom_option(:verbose), do: {:verbose, true}
  defp atom_option(:return_warnings), do: {:return_warnings, true}
  defp atom_option(:deterministic), do: {:deterministic, true}
  defp atom_option(key), do: key

  defp non_white(s) do
    s
    |> Enum.filter(fn c ->
      # $\200 or $\240
      c > ?\s and (c < 200 or c > 240)
    end)
  end

  defp collect_action(_ifile, {:error, _}, l, _cont0) do
    {:error, {l, :leex, :cannot_parse}, :ignored_end_line}
  end

  defp collect_action(ifile, chars, l0, cont0) do
    case :erl_scan.tokens(cont0, chars, l0) do
      {:done, {:ok, toks, _}, _} ->
        {:ok, toks, l0}

      {:done, {:eof, _}, _} ->
        {:eof, l0}

      {:done, {:error, e, _}, _} ->
        {:error, e, l0}

      {:more, cont1} ->
        collect_action(ifile, :io.get_line(ifile, :leex), l0 + 1, cont1)
    end
  end

  defp parse_rule_regexp(re0, [{m, exp} | ms], st) do
    split = :re.split(re0, '\\{' ++ m ++ '\\}', [{:return, :list}, :unicode])
    re1 = :lists.append(:lists.join(exp, split))
    parse_rule_regexp(re1, ms, st)
  end

  defp parse_rule_regexp(re, [], st) do
    case re_parse(re, st) do
      {:ok, r} -> {:ok, r}
      {:error, e} -> {:error, {:regexp, e}}
    end
  end

  defp var_used(name, toks) do
    case :lists.keyfind(name, 3, toks) do
      {:var, _, ^name} -> true
      _ -> false
    end
  end

  defp build_nfa({:alt, res}, n, f, nfa) do
    build_nfa_alt(res, n, f, nfa)
  end

  defp build_nfa({:seq, res}, n, f, nfa) do
    build_nfa_seq(res, n, f, nfa)
  end

  defp build_nfa({:kclosure, re}, n0, f, nfa0) do
    {nfa1, n1, e1} = build_nfa(re, n0 + 1, n0, nfa0)
    e = n1

    {[
       %Leex.NfaState{no: f, edges: [{:epsilon, n0}, {:epsilon, e}]},
       %Leex.NfaState{no: e1, edges: [{:epsilon, n0}, {:epsilon, e}]} | nfa1
     ], n1 + 1, e}
  end

  defp build_nfa({:pclosure, re}, n0, f, nfa0) do
    {nfa1, n1, e1} = build_nfa(re, n0 + 1, n0, nfa0)
    e = n1

    {[
       %Leex.NfaState{no: f, edges: [{:epsilon, n0}]},
       %Leex.NfaState{no: e1, edges: [{:epsilon, n0}, {:epsilon, e}]} | nfa1
     ], n1 + 1, e}
  end

  defp build_nfa({:optional, re}, n0, f, nfa0) do
    {nfa1, n1, e1} = build_nfa(re, n0 + 1, n0, nfa0)
    e = n1

    {[
       %Leex.NfaState{no: f, edges: [{:epsilon, n0}, {:epsilon, e}]},
       %Leex.NfaState{no: e1, edges: [{:epsilon, e}]} | nfa1
     ], n1 + 1, e}
  end

  defp build_nfa({:char_class, cc}, n, f, nfa) do
    {[%Leex.NfaState{no: f, edges: [{pack_cc(cc), n}]} | nfa], n + 1, n}
  end

  defp build_nfa({:comp_class, cc}, n, f, nfa) do
    {[%Leex.NfaState{no: f, edges: [{comp_class(cc), n}]} | nfa], n + 1, n}
  end

  defp build_nfa({:lit, cs}, n, f, nfa) do
    build_nfa_lit(cs, n, f, nfa)
  end

  defp build_nfa(:epsilon, n, f, nfa) do
    {[%Leex.NfaState{no: f, edges: [{:epsilon, n}]} | nfa], n + 1, n}
  end

  defp build_dfa(set, us, n, ts, ms, nfa) do
    set
    |> Enum.flat_map(fn s ->
      :erlang.element(s, nfa).edges
    end)
    |> Enum.filter(fn {crs, _st} ->
      crs != :epsilon
    end)
    |> Enum.flat_map(fn {crs, _st} -> crs end)
    |> :lists.usort()
    |> disjoint_crs()
    |> build_dfa(set, us, n, ts, ms, nfa)
  end

  defp accept([st | sts], nfa) do
    case :erlang.element(st, nfa) do
      %Leex.NfaState{accept: {:accept, a}} -> {:accept, a}
      %Leex.NfaState{accept: :noaccept} -> accept(sts, nfa)
    end
  end

  defp accept([], _), do: :noaccept

  defp min_delete([%Leex.DfaState{no: n, trans: t, accept: a} | dfa], t, a, new_n, rs, mdfa) do
    min_delete(dfa, t, a, new_n, [{n, new_n} | rs], mdfa)
  end

  defp min_delete([d | dfa], t, a, new_n, rs, mdfa) do
    min_delete(dfa, t, a, new_n, rs, [d | mdfa])
  end

  defp min_delete([], _, _, _, rs, mdfa), do: {mdfa, rs}

  defp file_copy(from, to) do
    case :io.get_line(from, :leex) do
      :eof ->
        :ok

      line when is_list(line) ->
        :io.fwrite(to, '~ts', [line])
        file_copy(from, to)
    end
  end

  defp out_trans(file, %Leex.DfaState{no: n, trans: [], accept: {:accept, a}}) do
    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, _, _) ->~n', [n])
    :io.fwrite(file, '    {~w,Tlen,Ics,Line};~n', [a])
  end

  defp out_trans(file, %Leex.DfaState{no: n, trans: tr, accept: {:accept, a}}) do
    tr |> pack_trans() |> Enum.each(fn t -> out_accept_tran(file, n, a, t) end)
    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, _, _) ->~n', [n])
    :io.fwrite(file, '    {~w,Tlen,Ics,Line,~w};~n', [a, n])
  end

  defp out_trans(file, %Leex.DfaState{no: n, trans: tr, accept: :noaccept}) do
    tr |> pack_trans() |> Enum.each(fn t -> out_noaccept_tran(file, n, t) end)
    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, Action, Alen) ->~n', [n])
    :io.fwrite(file, '    {Action,Alen,Tlen,Ics,Line,~w};~n', [n])
  end

  defp prep_out_actions(as) do
    as
    |> Enum.map(fn
      {a, :empty_action} ->
        {a, :empty_action}

      {a, code, token_chars, token_len, token_line} ->
        vars =
          [
            {token_chars, 'TokenChars'},
            {token_len, 'TokenLen'},
            {token_line, 'TokenLine'},
            {token_chars, 'YYtcs'},
            {token_len or token_chars, 'TokenLen'}
          ]
          |> Enum.map(fn {f, s} ->
            if f do
              s
            else
              '_'
            end
          end)

        name = :erlang.list_to_atom(:lists.concat([:yyaction_, a]))
        [chars, len, line, _, _] = vars
        args = Enum.filter([chars, len, line], fn v -> v != '_' end)
        args_chars = :lists.join(', ', args)
        {a, code, vars, name, args, args_chars}
    end)
  end

  defp out_action(file, {a, :empty_action}) do
    :io.fwrite(file, 'yyaction(~w, _, _, _) -> skip_token;~n', [a])
  end

  defp out_action(file, {a, _code, vars, name, _args, args_chars}) do
    [_, _, line, tcs, len] = vars
    :io.fwrite(file, 'yyaction(~w, ~s, ~s, ~s) ->~n', [a, len, tcs, line])

    if tcs != '_' do
      :io.fwrite(file, '    TokenChars = yypre(YYtcs, TokenLen),~n', [])
    end

    :io.fwrite(file, '    ~s(~s);~n', [name, args_chars])
  end

  defp out_action_code(_file, _xrl_file, _deterministic, {_a, :empty_action}), do: :ok

  defp out_action_code(file, xrl_file, deterministic, {_a, code, _vars, name, args, args_chars}) do
    :io.fwrite(file, '\n-compile({inline,~w/~w}).\n', [name, length(args)])
    l = :erl_scan.line(hd(code))
    output_file_directive(file, xrl_file, deterministic, l - 2)
    :io.fwrite(file, '~s(~s) ->~n', [name, args_chars])
    :io.fwrite(file, '    ~ts\n', [pp_tokens(code, l, file)])
  end

  defp quote1(?^, _file), do: '\\^'
  defp quote1(?., _file), do: '\\.'
  defp quote1(?$, _file), do: '\\$'
  defp quote1(?-, _file), do: '\\-'
  defp quote1(?[, _file), do: '\\['
  defp quote1(?], _file), do: '\\]'
  defp quote1(?\s, _file), do: '\\\\s'
  defp quote1(?\', _file), do: '\\\"'
  defp quote1(?\b, _file), do: '\\\\b'
  defp quote1(?\f, _file), do: '\\\\f'
  defp quote1(?\n, _file), do: '\\\\n'
  defp quote1(?\r, _file), do: '\\\\r'
  defp quote1(?\t, _file), do: '\\\\t'
  defp quote1(?\e, _file), do: '\\\\e'
  defp quote1(?\v, _file), do: '\\\\v'
  defp quote1(?\d, _file), do: '\\\\d'
  defp quote1(?\\, _file), do: '\\\\'

  defp quote1(c, file) when is_integer(c) do
    case enc(file) do
      :unicode -> :io_lib.write_char(c)
      :latin1 -> :io_lib.write_char_as_latin1(c)
    end
    |> case do
      [?$, ?\\ | cs] -> '\\\\' ++ cs
      [?$ | cs] -> cs
    end
  end

  defp quote1(:maxchar, _file), do: 'MAXCHAR'

  defp re_parse(cs0, st) do
    # case catch
    try do
      case re_reg(cs0, 0, st) do
        {re, _, []} -> {:ok, re}
        {_, _, [c | _]} -> {:error, {:illegal_char, [c]}}
      end
    catch
      {:parse_error, e} -> {:error, e}
    end
  end

  defp re_reg(cs, sn, st), do: re_alt(cs, sn, st)

  defp re_alt(cs0, sn0, st) do
    {l, sn1, cs1} = re_seq(cs0, sn0, st)

    case re_alt1(cs1, sn1, st) do
      {[], sn2, cs2} -> {l, sn2, cs2}
      {rs, sn2, cs2} -> {{:alt, [l | rs]}, sn2, cs2}
    end
  end

  defp re_alt1([?| | cs0], sn0, st) do
    {l, sn1, cs1} = re_seq(cs0, sn0, st)
    {rs, sn2, cs2} = re_alt1(cs1, sn1, st)
    {[l | rs], sn2, cs2}
  end

  defp re_alt1(cs, sn, _), do: {[], sn, cs}

  defp re_seq(cs0, sn0, st) do
    case re_seq1(cs0, sn0, st) do
      {[], sn1, cs1} -> {:epsilon, sn1, cs1}
      {[r], sn1, cs1} -> {r, sn1, cs1}
      {rs, sn1, cs1} -> {{:seq, rs}, sn1, cs1}
    end
  end

  # todo all when or should be checked
  defp re_seq1([c | _] = cs0, sn0, st) when c != ?| and c != ?) do
    {l, sn1, cs1} = re_repeat(cs0, sn0, st)
    {rs, sn2, cs2} = re_seq1(cs1, sn1, st)
    {[l | rs], sn2, cs2}
  end

  defp re_seq1(cs, sn, _), do: {[], sn, cs}

  defp re_repeat(cs0, sn0, st) do
    {s, sn1, cs1} = re_single(cs0, sn0, st)
    re_repeat1(cs1, sn1, s, st)
  end

  defp re_repeat1([?* | cs], sn, s, st), do: re_repeat1(cs, sn, {:kclosure, s}, st)
  defp re_repeat1([?+ | cs], sn, s, st), do: re_repeat1(cs, sn, {:pclosure, s}, st)
  defp re_repeat1([?? | cs], sn, s, st), do: re_repeat1(cs, sn, {:optional, s}, st)
  defp re_repeat1(cs, sn, s, _), do: {s, sn, cs}

  defp re_single([?( | cs0], sn0, st) do
    sn1 = sn0 + 1

    case re_reg(cs0, sn1, st) do
      {s, sn2, [?) | cs1]} -> {s, sn2, cs1}
      _ -> parse_error({:unterminated, '('})
    end
  end

  defp re_single([?. | cs], sn, _), do: {{:comp_class, '\n'}, sn, cs}

  defp re_single('[^' ++ cs0, sn, st) do
    case re_char_class(cs0, st) do
      {cc, [?] | cs1]} -> {{:comp_class, cc}, sn, cs1}
      _ -> parse_error({:unterminated, '['})
    end
  end

  defp re_single([?[ | cs0], sn, st) do
    case re_char_class(cs0, st) do
      {cc, [?] | cs1]} -> {{:char_class, cc}, sn, cs1}
      _ -> parse_error({:unterminated, '['})
    end
  end

  defp re_single([?\\ | cs0], sn, _) do
    {c, cs1} = re_char(?\\, cs0)
    {{:lit, [c]}, sn, cs1}
  end

  defp re_single([c | cs0], sn, st) do
    if special_char(c, st) do
      parse_error({:illegal_char, [c]})
    else
      {c, cs1} = re_char(c, cs0)
      {{:lit, [c]}, sn, cs1}
    end
  end

  defp build_nfa_alt([re], n, f, nfa), do: build_nfa(re, n, f, nfa)

  defp build_nfa_alt([re | r_es], n0, f, nfa0) do
    {nfa1, n1, e1} = build_nfa(re, n0 + 1, n0, nfa0)
    {nfa2, n2, e2} = build_nfa_alt(r_es, n1 + 1, n1, nfa1)
    e = n2

    {[
       %Leex.NfaState{no: f, edges: [{:epsilon, n0}, {:epsilon, n1}]},
       %Leex.NfaState{no: e1, edges: [{:epsilon, e}]},
       %Leex.NfaState{no: e2, edges: [{:epsilon, e}]} | nfa2
     ], n2 + 1, e}
  end

  defp build_nfa_seq(res, n0, f0, nfa0) do
    :lists.foldl(fn re, {nfa, n, f} -> build_nfa(re, n, f, nfa) end, {nfa0, n0, f0}, res)
  end

  defp pack_cc(cc) do
    :lists.foldl(
      fn
        {:range, cf, cl}, set -> :ordsets.add_element({cf, cl}, set)
        c, set -> :ordsets.add_element({c, c}, set)
      end,
      :ordsets.new(),
      cc
    )
    |> pack_crs()
  end

  defp pack_crs([{c1, c2} = cr, {c3, c4} | crs]) when c1 <= c3 and c2 >= c4,
    do: pack_crs([cr | crs])

  defp pack_crs([{c1, c2}, {c3, c4} | crs]) when c2 >= c3 and c2 < c4,
    do: pack_crs([{c1, c4} | crs])

  defp pack_crs([{c1, c2}, {c3, c4} | crs]) when c2 + 1 == c3, do: pack_crs([{c1, c4} | crs])
  defp pack_crs([cr | crs]), do: [cr | pack_crs(crs)]
  defp pack_crs([]), do: []

  defp comp_class(cc) do
    cc
    |> pack_cc()
    |> comp_crs(0)
  end

  defp comp_crs([{0, c2} | crs], 0), do: comp_crs(crs, c2 + 1)
  defp comp_crs([{c1, c2} | crs], last), do: [{last, c1 - 1} | comp_crs(crs, c2 + 1)]
  defp comp_crs([], last), do: [{last, :maxchar}]

  defp build_nfa_lit(cs, n0, f0, nfa0) do
    :lists.foldl(
      fn c, {nfa, n, f} ->
        {[%Leex.NfaState{no: f, edges: [{[{c, c}], n}]} | nfa], n + 1, n}
      end,
      {nfa0, n0, f0},
      cs
    )
  end

  defp disjoint_crs([{_c1, c2} = cr1, {c3, _c4} = cr2 | crs]) when c2 < c3 do
    [cr1 | disjoint_crs([cr2 | crs])]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 != c3 do
    [{c1, c2} | disjoint_crs(:ordsets.add_element({c2 + 1, c4}, crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 or c2 >= c3 or c2 < c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.union([{c3, c2}, {c2 + 1, c4}], crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 or c2 != c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.add_element({c3, c4}, crs))]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 < c3 or c2 > c4 do
    [{c1, c3 - 1} | disjoint_crs(:ordsets.union([{c3, c4}, {c4 + 1, c2}], crs))]
  end

  defp disjoint_crs([cr | crs]), do: [cr | disjoint_crs(crs)]
  defp disjoint_crs([]), do: []

  defp build_dfa([cr | crs], set, us, n, ts, ms, nfa) do
    case eclosure(move(set, cr, nfa), nfa) do
      s when s != [] ->
        case dfa_state_exist(s, us, ms) do
          {:yes, t} ->
            build_dfa(crs, set, us, n, :orddict.store(cr, t, ts), ms, nfa)

          :no ->
            u = %Leex.DfaState{no: n, nfa: s}
            build_dfa(crs, set, [u | us], n + 1, :orddict.store(cr, n, ts), ms, nfa)
        end

      [] ->
        build_dfa(crs, set, us, n, ts, ms, nfa)
    end
  end

  defp build_dfa([], _, us, n, ts, _, _), do: {ts, us, n}

  defp out_accept_tran(file, n, a, {{cf, :maxchar}, s}) do
    out_accept_head_max(file, n, cf)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_accept_tran(file, n, a, {{cf, cl}, s}) do
    out_accept_head_range(file, n, cf, cl)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_accept_tran(file, n, a, {?\n, s}) do
    out_accept_head_1(file, n, ?\n)
    out_accept_body(file, s, 'Line+1', a)
  end

  defp out_accept_tran(file, n, a, {c, s}) do
    out_accept_head_1(file, n, c)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_noaccept_tran(file, n, {{cf, :maxchar}, s}) do
    out_noaccept_head_max(file, n, cf)
    out_noaccept_body(file, s, 'Line')
  end

  defp out_noaccept_tran(file, n, {{cf, cl}, s}) do
    out_noaccept_head_range(file, n, cf, cl)
    out_noaccept_body(file, s, 'Line')
  end

  defp out_noaccept_tran(file, n, {?\n, s}) do
    out_noaccept_head_1(file, n, ?\n)
    out_noaccept_body(file, s, 'Line+1')
  end

  defp out_noaccept_tran(file, n, {c, s}) do
    out_noaccept_head_1(file, n, c)
    out_noaccept_body(file, s, 'Line')
  end

  defp pp_tokens(tokens, line0, file), do: pp_tokens(tokens, line0, file, :none)

  defp pp_tokens([], _line0, _, _), do: []

  defp pp_tokens([t | ts], line0, file, prev) do
    line = :erl_scan.line(t)
    [pp_sep(line, line0, prev, t), pp_symbol(t, file) | pp_tokens(ts, line, file, t)]
  end

  defp enc(file) do
    case :lists.keyfind(:encoding, 1, :io.getopts(file)) do
      false -> :latin1
      {:encoding, enc} -> enc
    end
  end

  defp parse_error(e), do: :erlang.throw({:parse_error, e})

  defp re_char_class([?] | cs], st), do: re_char_class(cs, [?]], st)
  defp re_char_class(cs, st), do: re_char_class(cs, [], st)

  defp re_char_class([c1 | cs0], cc, st) when c1 != ?] do
    case re_char(c1, cs0) do
      {cf, [?-, c2 | cs1]} when c2 != ?] ->
        case re_char(c2, cs1) do
          {cl, cs2} when cf < cl ->
            re_char_class(cs2, [{:range, cf, cl} | cc], st)

          {_, cs2} ->
            parse_error({:char_class, string_between([c1 | cs0], cs2)})
        end

      {c, cs1} ->
        re_char_class(cs1, [c | cc], st)
    end
  end

  defp re_char_class(cs, cc, _), do: {:lists.reverse(cc), cs}

  defp re_char(?\\, [o1, o2, o3 | s])
       when o1 >= ?0 and o1 <= ?7 and o2 >= ?0 and o2 <= ?7 and o3 >= ?0 and o3 <= ?7 do
    {(o1 * 8 + o2) * 8 + o3 - 73 * ?0, s}
  end

  defp re_char(?\\, [?x, h1, h2 | s]) when is_hex(h1) and is_hex(h2) do
    {:erlang.list_to_integer([h1, h2], 16), s}
  end

  defp re_char(?\\, [?x, ?{ | s0]), do: re_hex(s0, [])
  defp re_char(?\\, [?x | _]), do: parse_error({:illegal_char, '\\x'})
  defp re_char(?\\, [c | s]), do: {escape_char(c), s}
  defp re_char(?\\, []), do: parse_error({:unterminated, '\\'})
  defp re_char(c, s), do: {c, s}

  defp special_char(?^, _), do: true
  defp special_char(?., _), do: true
  defp special_char(?[, _), do: true
  defp special_char(?$, _), do: true
  defp special_char(?(, _), do: true
  defp special_char(?), _), do: true
  defp special_char(?|, _), do: true
  defp special_char(?*, _), do: true
  defp special_char(?+, _), do: true
  defp special_char(??, _), do: true
  defp special_char(?\\, _), do: true
  defp special_char(_, _), do: false

  defp move(sts, cr, nfa) do
    sts
    |> Enum.flat_map(fn n ->
      :erlang.element(n, nfa).edges
      |>IO.inspect()
    end)
    |> Enum.filter(fn {crs, _st} ->
      crs != :epsilon and in_crs(cr, crs)
    end)
    |>IO.inspect()
    |> Enum.map(fn {_crs, st} -> st end)
    |>IO.inspect()
  end

  defp dfa_state_exist(s, us, ms) do
    s|>IO.inspect()
    case :lists.keyfind(s, & &1.nfa, us) do
      %Leex.DfaState{no: t} ->
        {:yes, t}

      false ->
        case :lists.keyfind(s, & &1.nfa, ms) do
          %Leex.DfaState{no: t} -> {:yes, t}
          false -> :no
        end
    end
  end

  defp out_accept_head_max(file, state, min), do: out_head_max(file, state, min, '_', '_')

  defp out_accept_body(file, next, line, action) do
    out_body(file, next, line, :io_lib.write(action), 'Tlen')
  end

  defp out_accept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, '_', '_')
  end

  defp out_accept_head_1(file, state, char) do
    out_head_1(file, state, char, '_', '_')
  end

  defp out_noaccept_head_max(file, state, min) do
    out_head_max(file, state, min, 'Action', 'Alen')
  end

  defp out_noaccept_body(file, next, line) do
    out_body(file, next, line, 'Action', 'Alen')
  end

  defp out_noaccept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, 'Action', 'Alen')
  end

  defp out_noaccept_head_1(file, state, char) do
    out_head_1(file, state, char, 'Action', 'Alen')
  end

  defp pp_sep(line, line00, prev, t) when line > line00 do
    ['\n    ' | pp_sep(line - 1, line00, prev, t)]
  end

  defp pp_sep(_, _, {'.', _}, _), do: ''
  defp pp_sep(_, _, {'#', _}, _), do: ''
  defp pp_sep(_, _, {'(', _}, _), do: ''
  defp pp_sep(_, _, {'[', _}, _), do: ''
  defp pp_sep(_, _, _, {'.', _}), do: ''
  defp pp_sep(_, _, _, {'#', _}), do: ''
  defp pp_sep(_, _, _, {',', _}), do: ''
  defp pp_sep(_, _, _, {')', _}), do: ''
  defp pp_sep(_, _, _, _), do: ' '

  defp pp_symbol({:var, _, var}, _), do: :erlang.atom_to_list(var)
  defp pp_symbol({_, _, symbol}, file), do: format_symbol(symbol, file)
  defp pp_symbol({:dot, _}, _), do: '.'
  defp pp_symbol({symbol, _}, _), do: :erlang.atom_to_list(symbol)

  defp string_between(cs1, cs2) do
    :string.slice(cs1, 0, :string.length(cs1) - :string.length(cs2))
  end

  defp re_hex([c | cs], l) when is_hex(c), do: re_hex(cs, [c | l])

  defp re_hex([?} | s], l) do
    l = :lists.reverse(l)

    case :erlang.list_to_integer(l, 16) do
      c when c <= 0x10FFFF -> {c, s}
      _ -> parse_error({:illegal_char, [?\\, ?x, ?{ | l] ++ '}'})
    end
  end

  defp re_hex(_, _), do: parse_error({:unterminated, '\\x{'})

  defp escape_char(?n), do: ?\n
  defp escape_char(?r), do: ?\r
  defp escape_char(?t), do: ?\t
  defp escape_char(?v), do: ?\v
  defp escape_char(?b), do: ?\b
  defp escape_char(?f), do: ?\f
  defp escape_char(?e), do: ?\e
  defp escape_char(?s), do: ?\s
  defp escape_char(?d), do: ?\d
  defp escape_char(c), do: c

  defp in_crs({c1, c2}, [{c3, c4} | _crs]) when c1 >= c3 and c2 <= c4, do: true
  defp in_crs(cr, [cr | _crs]), do: true
  defp in_crs(cr, [_ | crss]), do: in_crs(cr, crss)
  defp in_crs(_cr, []), do: false

  defp out_head_max(file, state, min, action, alen) do
    :io.fwrite(file, 'yystate(~w, [C|Ics], Line, Tlen, ~s, ~s) when C >= ~w ->\n', [
      state,
      action,
      alen,
      min
    ])
  end

  defp out_body(file, next, line, action, alen) do
    :io.fwrite(file, '    yystate(~w, Ics, ~s, Tlen+1, ~s, ~s);\n', [next, line, action, alen])
  end

  defp out_head_range(file, state, min, max, action, alen) do
    :io.fwrite(file, 'yystate(~w, [C|Ics], Line, Tlen, ~s, ~s) when C >= ~w, C =< ~w ->\n', [
      state,
      action,
      alen,
      min,
      max
    ])
  end

  defp out_head_1(file, state, char, action, alen) do
    :io.fwrite(file, 'yystate(~w, [~w|Ics], Line, Tlen, ~s, ~s) ->\n', [state, char, action, alen])
  end

  defp format_symbol(symbol, file) do
    case enc(file) do
      :latin1 -> '~p'
      :unicode -> '~tp'
    end
    |> :io_lib.fwrite([symbol])
  end

  defp out_file(ifile, ofile, st, dfa, df, actions, code, l) do
    deterministic = :proplists.get_bool(:deterministic, st.opts)

    case :io.get_line(ifile, :leex) do
      :eof ->
        output_file_directive(ofile, st.ifile, deterministic, l)

      {:error, _} ->
        add_error(st.ifile, {l, :leex, :cannot_parse}, st)

      line ->
        case :string.slice(line, 0, 5) do
          '##mod' -> out_module(ofile, st)
          '##cod' -> out_erlang_code(ofile, st, code, l)
          '##dfa' -> out_dfa(ofile, st, dfa, code, df, l)
          '##act' -> out_actions(ofile, st.xfile, deterministic, actions)
          _ -> :io.put_chars(ofile, line)
        end

        out_file(ifile, ofile, st, dfa, df, actions, code, l + 1)
    end
  end
end

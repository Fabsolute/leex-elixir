defmodule Leex.Util.Writer do
  alias Leex.Util
  def out_file(state, dfa, dfa_first, actions, code) do
    case open_inc_file(state) do
      {:ok, ifile} ->
        try do
          case open_ex_file(state) do
            {:ok, efile} ->
              # todo removed encodings
              try do
                deterministic = Keyword.get(state.opts, :deterministic, false)
                output_file_directive(efile, state.ifile, deterministic, 0)
                out_file(ifile, efile, state, dfa, dfa_first, actions, code, 1)
                state
              after
                :ok = File.close(efile)
              end

            {:error, error} ->
              Util.add_error({:none, :leex, {:file_error, error}}, state)
          end
        after
          :ok = File.close(ifile)
        end

      {{:error, error}, ifile} ->
        Util.add_error(ifile, {:none, :leex, {:file_error, error}}, state)
    end
  end

  defp output_file_directive(_file, _filename, _deterministic, _line), do: :ok

  defp out_file(ifile, ofile, state, dfa, dfa_first, actions, code, line_number) do
    deterministic = Keyword.get(state.opts, :deterministic, false)

    case IO.gets(ifile, :leex) do
      :eof ->
        output_file_directive(ofile, state.ifile, deterministic, line_number)

      {:error, _} ->
        Util.add_error(state.ifile, {line_number, :leex, :cannot_parse}, state)

      line ->
        case String.slice(line, 0, 5) do
          "@@mod" -> out_module(ofile, state)
          "@@cod" -> out_erlang_code(ofile, state, code, line_number)
          "@@dfa" -> out_dfa(ofile, state, dfa, code, dfa_first, line_number)
          "@@act" -> out_actions(ofile, actions)
          _ -> IO.write(ofile, line)
        end

        out_file(ifile, ofile, state, dfa, dfa_first, actions, code, line_number + 1)
    end
  end

  defp out_module(file, state) do
    IO.binwrite(file, "defmodule #{Macro.camelize(Atom.to_string(state.module))} do\n")
  end

  defp out_erlang_code(file, state, code, line) do
    {code_line, code_pos, _n_code_lines} = code
    deterministic = Keyword.get(state.opts, :deterministic, false)
    output_file_directive(file, state.xfile, deterministic, code_line)
    {:ok, xfile} = open_xex_file(state)

    try do
      :file.position(xfile,code_pos)
      :ok =
        xfile
        |> file_copy(file)
    after
      :ok = File.close(xfile)
    end

    IO.binwrite(file, "\n")
    output_file_directive(file, state.ifile, deterministic, line)
  end

  defp out_dfa(file, state, dfa, {_code_line, _code_pos, n_code_lines}, dfa_first, line) do
    deterministic = Keyword.get(state.opts, :deterministic, false)
    output_file_directive(file, state.efile, deterministic, line + (n_code_lines - 1) + 3)
    IO.binwrite(file, "defp yystate(), do: #{dfa_first}\n\n")
    Enum.each(dfa, &out_trans(file, &1))

    IO.binwrite(file, """
    defp yystate(s, ics, line, tlen, action, alen) do
      {action,alen,tlen,ics,line,s}
    end
    """)
  end

  defp out_actions(file, actions) do
    actions = prep_out_actions(actions)
    Enum.each(actions, &out_action(file, &1))
    IO.binwrite(file, "defp yyaction(_, _, _, _), do: :error\n")
    Enum.each(actions, &out_action_code(file, &1))
  end

  defp out_action(file, {action, :empty_action}) do
    IO.binwrite(file, "defp yyaction(#{action}, _, _, _), do: :skip_token\n")
  end

  defp out_action(file, {action, _code, [_, _, line, tcs, len], name, _args, args_string}) do
    IO.binwrite(file, "defp yyaction(#{action}, #{len},#{tcs}, #{line}) do\n")

    if tcs != "_" do
      IO.binwrite(file, "    token_val = yypre(yy_tcs, token_len)\n")
    end

    IO.binwrite(file, "    #{name}(#{args_string})\nend\n")
  end

  defp out_action_code(_file, {_action, :empty_action}), do: :ok

  defp out_action_code(
         file,
         {_action, code, _vars, name, _args, args_string}
       ) do
    # todo removed compile inline
    # io:fwrite(File, "\n-compile({inline,~w/~w}).\n", [Name, length(Args)]),
    IO.binwrite(file, "defp #{name}(#{args_string}) do\n")
    IO.binwrite(file, "    #{Macro.to_string(code)}\nend\n")
  end

  defp prep_out_actions(actions) do
    actions
    |> Enum.map(fn
      {action, :empty_action} ->
        {action, :empty_action}

      {action, code, token_val, token_len, token_line} ->
        vars =
          [
            {token_val, "token_val"},
            {token_len, "token_len"},
            {token_line, "token_line"},
            {token_val, "yy_tcs"},
            {token_len or token_val, "token_len"}
          ]
          |> Enum.map(fn {f, s} ->
            if f do
              s
            else
              "_"
            end
          end)

        name = String.to_atom("yy_action_#{action}")
        [val, len, line, _, _] = vars
        args = for v <- [val, len, line], v != "_", do: v
        args_string = Enum.join(args, ", ")
        {action, code, vars, name, args, args_string}
    end)
  end

  defp out_trans(file, %Leex.DfaState{no: no, trans: [], accept: {:accept, accept}}) do
    IO.binwrite(file, """
    defp yystate(#{no}, ics, line, tlen, _, _) do
      {#{accept},tlen,ics,line}
    end
    """)
  end

  defp out_trans(file, %Leex.DfaState{no: no, trans: trans, accept: {:accept, accept}}) do
    trans |> pack_trans() |> Enum.each(&out_accept_tran(file, no, accept, &1))

    IO.binwrite(file, """
    defp yystate(#{no}, ics, line, tlen, _, _) do
      {#{accept},tlen,ics,line,#{no}}
    end
    """)
  end

  defp out_trans(file, %Leex.DfaState{no: no, trans: trans, accept: :noaccept}) do
    trans |> pack_trans() |> Enum.each(&out_noaccept_tran(file, no, &1))

    IO.binwrite(file, """
    defp yystate(#{no}, ics, line, tlen, action, alen) do
      {action,alen,tlen,ics,line,#{no}}
    end
    """)
  end

  defp out_accept_tran(file, next_state, action, {{char, :maxchar}, state}) do
    out_accept_head_max(file, next_state, char)
    out_accept_body(file, state, "line", action)
  end

  defp out_accept_tran(file, next_state, action, {{char_1, char_2}, state}) do
    out_accept_head_range(file, next_state, char_1, char_2)
    out_accept_body(file, state, "line", action)
  end

  defp out_accept_tran(file, next_state, action, {?\n, state}) do
    out_accept_head_1(file, next_state, ?\n)
    out_accept_body(file, state, "line+1", action)
  end

  defp out_accept_tran(file, next_state, action, {char, state}) do
    out_accept_head_1(file, next_state, char)
    out_accept_body(file, state, "line", action)
  end

  defp out_accept_head_1(file, state, char) do
    out_head_1(file, state, char, "_", "_")
  end

  defp out_accept_head_max(file, state, min) do
    out_head_max(file, state, min, "_", "_")
  end

  defp out_accept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, "_", "_")
  end

  defp out_accept_body(file, next, line, action) do
    out_body(file, next, line, :io_lib.write(action), "tlen")
  end

  defp out_noaccept_tran(file, next_state, {{char, :maxchar}, state}) do
    out_noaccept_head_max(file, next_state, char)
    out_noaccept_body(file, state, "line")
  end

  defp out_noaccept_tran(file, next_state, {{char_1, char_2}, state}) do
    out_noaccept_head_range(file, next_state, char_1, char_2)
    out_noaccept_body(file, state, "line")
  end

  defp out_noaccept_tran(file, next_state, {?\n, state}) do
    out_noaccept_head_1(file, next_state, ?\n)
    out_noaccept_body(file, state, "line+1")
  end

  defp out_noaccept_tran(file, next_state, {char, state}) do
    out_noaccept_head_1(file, next_state, char)
    out_noaccept_body(file, state, "line")
  end

  defp out_noaccept_head_1(file, state, char) do
    out_head_1(file, state, char, "action", "alen")
  end

  defp out_noaccept_head_max(file, state, min) do
    out_head_max(file, state, min, "action", "alen")
  end

  defp out_noaccept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, "action", "alen")
  end

  defp out_noaccept_body(file, next, line) do
    out_body(file, next, line, "action", "alen")
  end

  defp out_head_1(file, state, char, action, alen) do
    IO.binwrite(file, "defp yystate(#{state}, <<#{char},ics::binary>>, line, tlen, #{action}, #{alen}) do\n")
  end

  defp out_head_max(file, state, min, action, alen) do
    IO.binwrite(
      file,
      "defp yystate(#{state}, <<c,ics::binary>>, line, tlen, #{action}, #{alen}) when c >= #{min} do\n"
    )
  end

  defp out_head_range(file, state, min, max, action, alen) do
    IO.binwrite(
      file,
      "defp yystate(#{state}, <<c,ics::binary>>, line, tlen, #{action}, #{alen}) when c >= #{min} and c <= #{max} do\n"
    )
  end

  defp out_body(file, next, line, action, alen) do
    IO.binwrite(file, "    yystate(#{next}, ics, #{line}, tlen+1, #{action}, #{alen})\nend\n")
  end

  defp pack_trans(trans), do: pack_trans(trans, [])

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

  defp file_copy(from, to) do
    case IO.gets(from, :leex) do
      :eof ->
        :ok

      line ->
        IO.binwrite(to, line)
        file_copy(from, to)
    end
  end

  defp open_xex_file(state) do
    xfile = state.xfile

    try do
      file = File.open!(xfile)
      {:ok, file}
    catch
      error -> {error, xfile}
    end
  end

  defp open_ex_file(state) do
    efile = state.efile

    try do
      file = File.open!(efile, [:write])
      {:ok, file}
    catch
      error -> {error, efile}
    end
  end

  defp open_inc_file(state) do
    ifile = state.ifile

    try do
      file = File.open!(ifile)
      {:ok, file}
    catch
      error -> {error, ifile}
    end
  end
end

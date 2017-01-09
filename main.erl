-module(main).
-export([start/1, init/1]).

start(FileName) ->
  spawn_link(?MODULE, init, [FileName]).

init(FileName) ->
  {Entry, Paths, Exits} = readlines(FileName),
  Pid = self(),
  lists:map(fun(Cell) -> cell:create(Cell) end, Paths),
  lists:map(fun(Cell) -> cell:create(exit, Cell, Pid) end, Exits),
  whereis(get_name(Entry)) ! {find, []},
  loop().


loop() ->
  receive
    {found, Path} ->
      io:format("Found! ~p~n", [Path]);
    _ -> io:format("Wild message~n"),
      loop()
  end.


readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  [Params, _] = re:replace(get_line(Device), "\\n", ""),
  [Size, X, Y] = [list_to_integer(X) || X <- string:tokens(binary_to_list(Params)," ")],
  Cells = get_cells(lists:reverse(get_line(Device, [])), Size),
  file:close(Device),
  Exits = lists:delete({X,Y}, get_exits(Cells, Size)),
  Paths = get_paths(Cells, Exits),
  {{X,Y}, Paths, Exits}.


get_line(Device) ->
  case file:read_line(Device) of
    {ok, Data} -> Data;
    eof -> [];
    {error, Reason} -> io:format("Error: ~p~n", [Reason])
  end.


get_line(Device, List) ->
  case file:read_line(Device) of
    {ok, Data} -> get_line(Device, lists:append([Data], List));
    eof -> List;
    {error, Reason} -> io:format("Error: ~p~n", [Reason])
  end.


get_paths(Cells, Exits) ->
  lists:subtract(Cells, Exits).


get_exits(Cells, Size) ->
  lists:filter(fun(Cell) -> is_edge(Cell, Size) end, Cells).


is_edge({X,Y}, Size) ->
  X =:= 0 orelse (X / Size) =:= 1.0 orelse Y =:= 0 orelse (Y / Size) =:= 1.0.


get_name({X,Y}) ->
  list_to_atom(integer_to_list(X) ++ "_" ++ integer_to_list(Y)).


get_cells(Lines, Size) ->
  Rows = get_rows(Lines, [], Size, Size),
  lists:filtermap(fun({Char, Cell}) -> case Char of 32 -> {true, Cell}; _ -> false end end, lists:flatten(Rows)).


get_rows(_Lines, Rows, _Size, 0) ->
  Rows;
get_rows([Row | Lines], Rows, Size, N) ->
  get_rows(Lines, [get_columns(Row, [], N, Size)| Rows], Size, N-1).


get_columns(_Row, Chars, _R, 0) ->
  Chars;
get_columns([Char | Row], Cells, R, N) ->
  Cell = {Char, {R,N}},
  get_columns(Row, [Cell | Cells], R, N-1).


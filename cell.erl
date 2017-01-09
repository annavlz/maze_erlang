-module(cell).
-export([create/1, create/3, loop/1, loop/2]).

create({X,Y}) ->
  Name = get_name({X,Y}),
  register(Name, spawn(?MODULE, loop, [{X,Y}])).

create(exit, {X,Y}, Server) ->
  Name = get_name({X,Y}),
  register(Name, spawn(?MODULE, loop, [{X,Y}, Server])).


loop({X,Y}) ->
  receive
    {find, Path} ->
      Coords = [{X,Y+1}, {X,Y-1}, {X+1,Y}, {X-1,Y}],
      Names = lists:map(fun(Coord) -> get_name(Coord) end, Coords),
      CellsRaw = lists:map(fun(Name) -> whereis(Name) end, Names),
      Cells = lists:filter(fun(Cell) -> Cell /= undefined end, CellsRaw),
      lists:map(fun(Cell) -> Cell ! {find, [{X,Y} | Path]} end, Cells);
    _ ->
      io:format("Wild message~n"),
      loop({X,Y})
  end.

loop({X,Y}, Server) ->
  receive
    {find, Path} ->
      Server ! {found, [{X,Y} | Path]};
    _ -> loop({X,Y}, Server)
  end.

get_name({X,Y}) ->
  list_to_atom(integer_to_list(X) ++ "_" ++ integer_to_list(Y)).
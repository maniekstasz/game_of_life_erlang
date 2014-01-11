-module(lifemain).
-compile(export_all).

-import(lifelogic,[createConstants/2,getBorders/5, setBorders/3]).

test(ColumnCount) ->
  {ok,Dir} = file:get_cwd(),
  %ColumnCount = 32,
  %StartG =now(),
  {BoardSize, Columns} = lifeio:readDataToColumns(Dir ++ '/fff.gz', ColumnCount),
%	StopG=now(),
%	GetTime = timer:now_diff(StopG, StartG),
  %Board = getBoardExtraTopAndBottom(PreBoard, BoardSize).


  ColumnWidth = BoardSize div ColumnCount,
  {InnerBoard, Left, Right, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),

  %Columns= split(ColumnCount, BoardSize, Board),
  ColumnSize = (ColumnWidth + 2) *(BoardSize+2),
  %	StartB =now(),
  Borders = lists:map(fun(Column) -> lifelogic:getBorders(Column, Left, Right, ColumnWidth, ColumnSize) end, Columns),
  BorderTuples = lifemain:prepareColumnData(Borders, Zero),


  ColumnsWithBorders = lists:map(fun(X) -> lifelogic:setBorders(InnerBoard, ColumnSize, X) end, BorderTuples),
  Supervisor = spawn(lifeconc,supervise,[ColumnCount, ColumnWidth, BoardSize, ColumnsWithBorders]),
  Supervisor ! start.
%StopB=now(),
%BorderTime = timer:now_diff(StopB, StartB),

%StartN =now(),
%Nexts = lists:map(fun(Elem) -> next(Elem, ColumnWidth+2, BoardSize+2) end, ColumnsWithBorders),
%StopN=now(),
%NextTime = timer:now_diff(StopN, StartN),
%{GetTime, BorderTime, NextTime}.
%Inners = lists:map(fun(Elem) -> getInnerBoard(Elem, ColumnWidth+2, BoardSize+2) end, Nexts),
%Glued = glue(Inners, ColumnWidth, BoardSize),
%lifeio:writeBoard(Glued, BoardSize,BoardSize).

test() ->

	Nodes = lifeconc:prepareNodes(),
	
	ColumnCount = erlang:length(Nodes), % liczba kolumn odpowiada liczbie węzłów
	{ok, Dir} = file:get_cwd(),
	{BoardSize, Columns} = lifeio:readDataToColumns(Dir ++ '/fff.gz', ColumnCount),
	ColumnWidth = BoardSize div ColumnCount,
	ColumnSize = (ColumnWidth+2) * (BoardSize+2),
	
	{InnerBoard, Left, Right, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),
	
	% od tego momentu iterujemy
	
	%Board = lifelogic:getBoardExtraTopAndBottom(PrevBoard, BoardSize),
	%Columns = lifelogic:split(ColumnCount, BoardSize, Board),
	
% teoretycznie od tego momentu można zacząć rozsyłać zadania na węzły
% trzeba jednak uwzględnić narzut danych
% bo chyba lepiej jest dać węzłowi jak najmniej danych (straty na przesyłach)
% za to do jak najcięższej pracy (przeliczenia)
	
	Borders = lists:map(fun(Column) -> lifelogic:getBorders(Column, Left, Right, ColumnWidth, ColumnSize) end, Columns),
	BorderTuples = prepareColumnData(Borders, Zero),
	ColumnsWithBorders = lists:map(fun(X) -> lifelogic:setBorders(InnerBoard, ColumnSize, X) end, BorderTuples),
	Nexts = lists:map(fun(Elem) -> lifelogic:next(Elem, ColumnWidth+2, BoardSize+2) end, ColumnsWithBorders),
	Inners = lists:map(fun(Elem) -> lifelogic:getInnerBoard(Elem, ColumnWidth+2, BoardSize+2) end, Nexts),
	
	Glued = lifelogic:glue(Inners, ColumnWidth, BoardSize),
	lifeio:writeBoard(Glued, BoardSize,BoardSize).

% przyjmuje tablicę krotek:
% {lewa krawedz kolumny, kolumna, prawa krawedz kolumny}
% zwraca tablicę z krotkami:
% {prawa krawędź poprzedniej kolumny, kolumna, lewa krawędź następnej kolumny}

prepareColumnData(List,Zeros) ->
	Elements = [emptystart] ++ List ++ [emptyend],
	ToTrim = lists:map(fun(Element) -> list_to_tuple(lists:sublist(Elements, indexOf(Element,Elements), 3)) end, Elements),
	Trimmed = lists:sublist(ToTrim, 1, erlang:length(Elements)-2),
	Res = lists:map(fun(Triple) -> parseColumnData(Triple, Zeros) end, Trimmed),
	Res.

parseColumnData(ColumnTuple, Zeros) -> 
	{Elem1,Elem2,Elem3} = ColumnTuple,
	case Elem1 of
		emptystart -> RightAsLeft = Zeros;
		_ -> {_,_,RightAsLeft} = Elem1
	end,
	{_,Col,_} = Elem2,
	case Elem3 of
		emptyend -> LeftAsRight = Zeros;
		_ -> {LeftAsRight,_,_} = Elem3
	end,
	{RightAsLeft,Col,LeftAsRight}.

% zwraca indeks objektu na liście

indexOf(Elem,List) -> indexOf(Elem,List, 1).
indexOf(_, [], _) -> {err, noelement};
indexOf(Elem, List, Index) ->
	case lists:nth(Index, List) == Elem of
		true -> Index;
		false -> indexOf(Elem,List,Index+1)
	end.
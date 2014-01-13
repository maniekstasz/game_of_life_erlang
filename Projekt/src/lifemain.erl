-module(lifemain).

-export([testTimeLocal/0]).

-export([localOperateColumns/3,calculateSingleColumn/3]).
-export([prepareColumnTuples/2,borderTuplesToColumnTriples/1,columnTripleToTuple/2]).
-export([indexOf/2,loadWholeBoard/1]).

-export([iterateLocal/3]).

-type board() :: bitstring().
-type column() :: bitstring().
-type columns() :: [column()].
-type border() :: integer().
-type columnTuple() :: {border(), column(), border()}.
-type columnTuples() :: [columnTuple()].
-type columnTriple() :: {columnTuple(),columnTuple(),columnTuple()}.
-type nodes() :: [node()].
-type key() :: pid().
-type nodeKey() :: {node(), key()}.
-type nodeKeys() :: [nodeKey()].





testRemote(Nodes, NumberOfProcesses) ->
  %Nodes = [],
  %Nodes = lifeconc:prepareNodes(),
  {BoardSize, Board} = lifemain:loadWholeBoard('/fff.gz'),
  %lifeio:writeBoard(Board, BoardSize, BoardSize),
  io:format("Start~n"),
  %FinalBoard = iterate(Board, BoardSize, Nodes, 1, 1),
  %countTimeForData(Board, BoardSize, Nodes, 128, 1),
  io:format("Stop~n").%,
%lifeio:writeBoard(FinalBoard, BoardSize, BoardSize).





%% @doc Metoda pojedynczej, calosciowej iteracji. Glowny board jest rozszerzany w pionie o zera, nastepnie jest dzielony
%%      na kolumny w liczbie liczba_wezlow * liczba_procesow_na_wezel, kazda porcja danych jest przesylana do osobnych
%%      wezlow, a po zakonczeniu obliczen, dane sa scalane.
-spec iterate(columns(), integer(), nodes(), integer(), integer()) -> columns().
iterate(Columns,_,_,_,0) -> Columns;
iterate(Columns, BoardSize, Nodes, ProcessCount, IterationCounter) ->
  %io:format("iteracja ~B~n", [IterationCounter]),
%% @TODO wyznaczyc najlepszy mnoznik dla kolumn - testy

  NodesCount = 1,%length(Nodes),
  ColumnsCount = length(Columns),
  ColumnWidth = BoardSize div ColumnsCount,

  ColumnTuples = prepareColumnTuples(Columns, BoardSize),
  %io:format("board podzielony na ~B kolumn~n", [ColumnsCount]),

  %NodeKeyList = lists:map(fun(X) ->
  %  NodeChunk = lists:sublist(ColumnTuples,(X-1)*ProcessCount+1, ProcessCount),
  %  Node = lists:nth(X,Nodes),
  %  initializeNode(Node, NodeChunk, BoardSize, ColumnWidth) end,
  %  lists:seq(1,NodesCount)),
  %io:format("Przygotowano porcje dla wezlow~n"),
  %io:format("Stworzono procesy i przeslano dane do: ~n"),
  %io:write(NodeKeyList),
  %io:format("~n"),

  _LocalResult = localOperateColumns(ColumnTuples, BoardSize, ColumnWidth),
  %_RemoteResult = remoteOperateColumns(NodeKeyList),

%% @TODO zamiana nodekeylist na tablice danych
  %ResultColumns = gatherResult(NodeKeyList, NodesCount),

  %Glued = lifelogic:glue(Result, ColumnWidth, BoardSize),
  iterate(_LocalResult, BoardSize, Nodes, ProcessCount, IterationCounter-1).


testTimeLocal() -> testTimeLocal(256).
testTimeLocal(0) -> ok;
testTimeLocal(Count) ->
  {LoadTime, Res} = timer:tc(lifeio, readDataToColumns, ['fff.gz', Count]),
  {BoardSize, Columns} = Res,
  io:format("Ladowanie tablicy (~B kolumn) o rozmiarze ~B w ciagu ~B mikrosekund~n", [length(Columns), BoardSize, LoadTime]),

  IterationsNumber = 1,
  {IterateTime,_} = timer:tc(lifemain, iterateLocal, [Columns, BoardSize, IterationsNumber]),
  io:format("~B procesow, ~B iteracji -> ~B mikrosekund~n", [Count, IterationsNumber, IterateTime]),
  NewCount = Count div 2,
  testTimeLocal(NewCount).


iterateLocal(Columns, _, 0) -> Columns;
iterateLocal(Columns, BoardSize, IterationCounter) ->
  io:format("Lokalna iteracja nr ~B~n", [IterationCounter]),
  ColumnsCount = length(Columns),
  ColumnWidth = BoardSize div ColumnsCount,
  ColumnTuples = prepareColumnTuples(Columns, BoardSize),
  Result = localOperateColumns(ColumnTuples, BoardSize, ColumnWidth),
  iterateLocal(Result, BoardSize, IterationCounter-1).



gatherResult(NodeKeyList, Count) -> gatherResult(NodeKeyList, Count, 0).
gatherResult(_, 0, Data) -> Data;
gatherResult(NodeKeyList, NodesLeft, Data) ->
  receive
    {ok, Node} ->
      {_, Key} = lists:keyfind(Node, 1, NodeKeyList),
      RemoteResult = rpc:yield(Key),
      io:format("- Dostalem od ~s klucz ~p; wartosc: ", [Node, Key]),
      io:write(RemoteResult),
      io:format("~n"),
      gatherResult(NodeKeyList, NodesLeft-1, Data);
    _Other ->
      io:format("Dostalem cos dziwnego:~n"),
      io:write(_Other),
      io:format("~n~n")
  after 15000 ->
    io:format("Timeout 15 sekund~n"),
    timeout
  end,
  Data.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda lokalnie przeliczajaca kolumny. Dostaje liste krotek kolumn do przetworzenia na maszynie, uzywajac metody
%%      rpc:pmap, przelicza dla kazdej kolumny nastepny stan, a nastepnie skleja wynikowe kolumny w jedna i zwraca ja.
%% ---------------------------------------------------------------------------------------------------------------------
-spec initializeNode(node(),columnTuples(),integer(),integer()) -> nodeKey().
initializeNode(Node, NodeChunk, BoardSize, ColumnWidth) ->
  NodeKey = conc:createNode(Node,self(),{lifemain,performLocalChunk,[NodeChunk,BoardSize,ColumnWidth]}),
  NodeKey.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda lokalnie przeliczajaca kolumny. Dostaje liste krotek kolumn do przetworzenia na maszynie, uzywajac metody
%%      rpc:pmap, przelicza dla kazdej kolumny nastepny stan i zwraca liste przeliczonych kolumn w tej samej kolejnosci.
%% ---------------------------------------------------------------------------------------------------------------------
-spec localOperateColumns(columnTuples(), integer(), integer()) -> column().
localOperateColumns(ColumnTuples, BoardSize, ColumnWidth) ->
  CalculatedColumns = rpc:pmap({lifemain,calculateSingleColumn},[BoardSize,ColumnWidth],ColumnTuples),
  CalculatedColumns.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przeliczajaca pojedyncza kolumne. Na podstawie krotki kolumny tworzona jest kolumna rozszerzona
%%      o krawedzie poprzedniej i nastepnej kolumny, przeliczane sa stany wszystkich komorek kolumny, a nastepnie
%%      kolumna jest obcinana ze zbednych elementow i zwracana.
%% ---------------------------------------------------------------------------------------------------------------------
-spec calculateSingleColumn(columnTuple(), integer(), integer()) -> column().
calculateSingleColumn(ColumnTuple, BoardSize, ColumnWidth) ->
  {InnerBoardConst,_,_,_} = lifelogic:createConstants(ColumnWidth, BoardSize),
  ExtendedColumnSize = (ColumnWidth+2) * (BoardSize+2),
  ColumnWithBorders = lifelogic:setBorders(InnerBoardConst, ExtendedColumnSize, ColumnTuple),
  Next = lifelogic:next(ColumnWithBorders, ColumnWidth+2, BoardSize+2),
  Inner = lifelogic:getInnerBoard(Next, ColumnWidth+2, BoardSize+2),
  Inner.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przygotowuje kolumny do przetwarzania. W jej wyniku powstaje lista krotek o postaci: {prawa krawedz
%%      poprzedniej kolumny (lub zero, jesli pierwsza kolumna), dana kolumna, lewa krawedz nastepnej kolumny (lub zero,
%%      jesli ostatnia kolumna)}. Takie struktury danych umozliwiaja niezalezne przeliczanie poszczegolnych kolumn
%%      w rozproszeniu.
%% ---------------------------------------------------------------------------------------------------------------------
-spec prepareColumnTuples(columns(), integer()) -> columnTuples().
prepareColumnTuples(Columns, BoardSize) ->
  ColumnWidth = BoardSize div length(Columns),
  ExtendedColumnSize = (ColumnWidth+2) * (BoardSize+2),
  {_, Left, Right, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),
  BorderTuples = lists:map(fun(Column) -> lifelogic:getBorders(Column, Left, Right, ColumnWidth, ExtendedColumnSize) end, Columns),
  ColumnTriplets = borderTuplesToColumnTriples(BorderTuples),
  FinalColumnTuples = lists:map(fun(ColumnTriple) ->
    columnTripleToTuple(ColumnTriple, Zero) end,
    ColumnTriplets),
  FinalColumnTuples.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda tworzaca trojke kolumn. Trojka kolumn to krotka zawierajaca 3 krotki kolumn - poprzedniej, biezacej oraz
%%      nastepnej kolumny. Metoda jest jednym z etapow tworzenia krotek kolumn - wygodnych do przetwarzania struktur.
%% ---------------------------------------------------------------------------------------------------------------------
borderTuplesToColumnTriples(BorderTuples) ->
  ExtendedBorderTuples = [blankstart] ++ BorderTuples ++ [blankend],
  ColumnTriplets = lists:map(fun(BorderTuple) ->
    ListOfThree = lists:sublist(ExtendedBorderTuples, indexOf(BorderTuple,ExtendedBorderTuples), 3),
    list_to_tuple(ListOfThree) end,
    ExtendedBorderTuples),
  TrimmedColumnTriplets = lists:sublist(ColumnTriplets, 1, length(ExtendedBorderTuples)-2),
  TrimmedColumnTriplets.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda tworzaca krotke kolumny. Krotka kolumny to krotka zawieracaca kolejno: prawa krawedz poprzedniej kolumny
%%      (lub zera, jesli pierwsza kolumna), dana kolumne, lewa krawedz nastepnej kolumny (lub zera, jesli ostatnia
%%%     kolumna). Metoda jest jednym z etapow tworzenia krotek kolumn - wygodnych do przetwarzania struktur.
%% ---------------------------------------------------------------------------------------------------------------------
-spec columnTripleToTuple(columnTriple(), bitstring()) -> columnTuple().
columnTripleToTuple(ColumnTriple, Zero) ->
  {PrevCol,CurrCol,NextCol} = ColumnTriple,
  case PrevCol of
    blankstart -> RightAsLeft = Zero;
    _ -> {_,_,RightAsLeft} = PrevCol
  end,
  {_,Col,_} = CurrCol,
  case NextCol of
    blankend -> LeftAsRight = Zero;
    _ -> {LeftAsRight,_,_} = NextCol
  end,
  {RightAsLeft,Col,LeftAsRight}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda pomocnicza. Zwraca indeks obiektu na liscie.
%% ---------------------------------------------------------------------------------------------------------------------
-spec indexOf(any(),list()) -> integer().
indexOf(Elem,List) -> indexOf(Elem,List, 1).
indexOf(_, [], _) -> {err, noelement};
indexOf(Elem, List, Index) ->
	case lists:nth(Index, List) == Elem of
		true -> Index;
		false -> indexOf(Elem,List,Index+1)
	end.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda pomocnicza. Laduje cala tablice z podanego pliku.
%% ---------------------------------------------------------------------------------------------------------------------
loadWholeBoard(Filename) ->
  {ok, Dir} = file:get_cwd(),
  lifeio:readDataToBoard(Dir ++ Filename).
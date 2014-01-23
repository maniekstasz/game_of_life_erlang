-module(lifemain).

-export([testTimeLocal/0,testTimeLocal/2]).

-export([calculateSingleColumn/4]).
-export([prepareColumnTuples/7,borderTuplesToColumnTriples/1,columnTripleToTuple/3,test/2, testRemote/2, indexOf/2]).

-export([iterateLocal/9]).

-type board() :: bitstring().
-type column() :: bitstring().
-type columns() :: [column()].
-type border() :: integer().
-type columnTuple() :: {border(), column(), border()}.
-type columnTuples() :: [columnTuple()].
-type columnTriple() :: {columnTuple(),columnTuple(),columnTuple()}.
-type columnTriples() :: [columnTriple()].
-type nodes() :: [node()].
-type key() :: pid().
-type nodeKey() :: {node(), key()}.
-type nodeKeys() :: [nodeKey()].



testRemote(ColumnsCount,Iteration) ->
	{BoardSize, Columns} = lifeio:readDataToColumns('/fff.gz', ColumnsCount),
	ColumnWidth = BoardSize div ColumnsCount,
	lifeconc:mainController(Columns, ColumnWidth, BoardSize, ColumnsCount, Iteration).
	%NewColumns = lifemain:iterateLocal(Columns, BoardSize, 4),
	%Inners = lists:map(fun(Elem) -> lifelogic:getInnerBoard(Elem, ColumnWidth+2, BoardSize+2) end, NewColumns),
	%Glued = lifelogic:glue(Inners, ColumnWidth, BoardSize),
	%Data = lifeio:writeBoardToFile(Glued, BoardSize).

test(ColumnsCount,Iteration) ->
	{BoardSize, Columns} = lifeio:readDataToColumns('/fff.gz', ColumnsCount),
	ColumnWidth = BoardSize div ColumnsCount,
	{InnerConstant, LeftConstant, RightConstant, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),
	Begin = now(),
 	NewColumns = lifemain:iterateLocal(Columns, ColumnWidth,BoardSize, ColumnsCount,Zero, Zero, LeftConstant, RightConstant,InnerConstant),
	io:format("~p~n", [timer:now_diff(now(), Begin)]),
	Inners = lists:map(fun(Elem) -> lifelogic:getInnerBoard(Elem, ColumnWidth+2, BoardSize+2) end, NewColumns),
	Glued = lifelogic:glue(Inners, ColumnWidth, BoardSize),
	%Data = lifeio:writeBoard(Glued, BoardSize, BoardSize),
	lifeio:writeBoardToFile(Glued, BoardSize).




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

  %ColumnTuples = prepareColumnTuples(Columns, BoardSize),
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

  %_LocalResult = localOperateColumns(ColumnTuples, BoardSize, ColumnWidth),
  %_RemoteResult = remoteOperateColumns(NodeKeyList),

%% @TODO zamiana nodekeylist na tablice danych
  %ResultColumns = gatherResult(NodeKeyList, NodesCount),
ok.
  %Glued = lifelogic:glue(Result, ColumnWidth, BoardSize),
  %iterate(_LocalResult, BoardSize, Nodes, ProcessCount, IterationCounter-1).




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


testTimeLocal() -> testTimeLocal(256, 10).
%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda testujaca wydajnosc lokalnej maszyny. Dla podanej liczby procesow i iteracji wykonuje obliczenia dla
%%      wszystkich kolejnych iteracji (od jednej do zadanej, krok N+1) i wszystkich mozliwych liczb procesow (od zadanej do
%%      jednego procesu, krok N/2).
%% ---------------------------------------------------------------------------------------------------------------------
-spec testTimeLocal(integer(), integer()) -> ok.
testTimeLocal(0,_) -> ok;
testTimeLocal(Count, IterationsNumber) ->
  {LoadTime, {BoardSize, Columns}} = timer:tc(lifeio, readDataToColumns, ['/fff.gz', Count]),
  io:format("liczba procesow: ~B, rozmiar planszy: ~B, czas ladowania: ~B~n[liczba iteracji, czas iteracji]~n",
    [Count, BoardSize, LoadTime]),
  lists:map(fun(X) ->
    {IterateTime,_} = timer:tc(lifemain, iterateLocal, [Columns, BoardSize, X]),
    io:format("~w~n", [[X, IterateTime]]) end,
    lists:seq(1,IterationsNumber)),
  io:format("~n"),
  NewCount = Count div 2,
  testTimeLocal(NewCount, IterationsNumber).


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda iterujaca na lokalnej maszynie. Z podanych kolumn tworzy krotki kolumn, ktore nastepnie rozsyla do
%%      Dostaje liste krotek kolumn do przetworzenia na maszynie, uzywajac metody
%%      rpc:pmap, przelicza dla kazdej kolumny nastepny stan i zwraca liste przeliczonych kolumn w tej samej kolejnosci.
%% ---------------------------------------------------------------------------------------------------------------------
-spec iterateLocal(columns(), integer(), integer(), integer(), bitstring(), bitstring(), integer(), integer(), integer()) -> columns().
%iterateLocal(Columns, _, 0) -> Columns;
%iterateLocal(Columns, BoardSize, IterationCounter) ->
 % ColumnsCount = length(Columns),
% % ColumnWidth = BoardSize div ColumnsCount,
%  ColumnTuples = prepareColumnTuples(Columns, BoardSize),
%  Result = rpc:pmap({lifemain,calculateSingleColumn},[BoardSize,ColumnWidth],ColumnTuples),
 % iterateLocal(Result, BoardSize, IterationCounter-1).
%

iterateLocal(Columns,ColumnWidth,Height, ColumnsCount, LeftAsRight, RightAsRight, LeftConstant, RightConstant, InnerBoardConst) ->
  ColumnTuples = prepareColumnTuples(Columns,ColumnWidth, Height,LeftAsRight, RightAsRight,LeftConstant, RightConstant),
  %lists:foreach(fun(ColumnTuple) -> ColumnWithBorders = calculateSingleColumn(ColumnTuple, Height, ColumnWidth), lifeio:writeBoard(ColumnWithBorders, 2+2, 8+2), io:format("~n") end,ColumnTuples ).
 rpc:pmap({lifemain,calculateSingleColumn},[Height,ColumnWidth, InnerBoardConst],ColumnTuples).


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przeliczajaca pojedyncza kolumne. Na podstawie krotki kolumny tworzona jest kolumna rozszerzona
%%      o krawedzie poprzedniej i nastepnej kolumny, przeliczane sa stany wszystkich komorek kolumny, a nastepnie
%%      kolumna jest obcinana ze zbednych elementow i zwracana.
%% ---------------------------------------------------------------------------------------------------------------------
-spec calculateSingleColumn(columnTuple(), integer(), integer(), integer()) -> column().
calculateSingleColumn(ColumnTuple, BoardSize, ColumnWidth, InnerBoardConst) ->
  %{InnerBoardConst,_,_,_} = lifelogic:createConstants(ColumnWidth, BoardSize),
  ExtendedColumnSize = (ColumnWidth+2) * (BoardSize+2),
  ColumnWithBorders = lifelogic:setBorders(InnerBoardConst, ExtendedColumnSize, ColumnTuple),
  Next = lifelogic:next(ColumnWithBorders, ColumnWidth+2, BoardSize+2),
  %Inner = lifelogic:getInnerBoard(Next, ColumnWidth+2, BoardSize+2),
  Next.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przygotowuje kolumny do przetwarzania. W jej wyniku powstaje lista krotek o postaci: {prawa krawedz
%%      poprzedniej kolumny (lub zero, jesli pierwsza kolumna), dana kolumna, lewa krawedz nastepnej kolumny (lub zero,
%%      jesli ostatnia kolumna)}. Takie struktury danych umozliwiaja niezalezne przeliczanie poszczegolnych kolumn
%%      w rozproszeniu.
%% ---------------------------------------------------------------------------------------------------------------------
-spec prepareColumnTuples(columns(), integer(),integer(), bitstring(), bitstring(), integer(), integer()) -> columnTuples().
prepareColumnTuples(Columns,ColumnWidth, BoardSize, LeftAsRight, RightAsRight, LeftConstant, RightConstant) ->
  %ColumnWidth = BoardSize div length(Columns),
  ExtendedColumnSize = (ColumnWidth+2) * (BoardSize+2),
%  {_, Left, Right, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),
  BorderTuples = lists:map(fun(Column) -> lifelogic:getBorders(Column, LeftConstant, RightConstant, ColumnWidth, ExtendedColumnSize) end, Columns),
  ColumnTriplets = borderTuplesToColumnTriples(BorderTuples),
  FinalColumnTuples = lists:map(fun(ColumnTriple) ->
    columnTripleToTuple(ColumnTriple, LeftAsRight, RightAsRight) end,
    ColumnTriplets),
  FinalColumnTuples.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda tworzaca trojke kolumn. Trojka kolumn to krotka zawierajaca 3 krotki kolumn - poprzedniej, biezacej oraz
%%      nastepnej kolumny. Metoda jest jednym z etapow tworzenia krotek kolumn - wygodnych do przetwarzania struktur.
%% ---------------------------------------------------------------------------------------------------------------------
-spec borderTuplesToColumnTriples(columnTuples()) -> columnTriples().
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
-spec columnTripleToTuple(columnTriple(), bitstring(), bitstring()) -> columnTuple().
columnTripleToTuple(ColumnTriple, LeftAsRightV, RightAsRightV) ->
  {PrevCol,CurrCol,NextCol} = ColumnTriple,
  case PrevCol of
    blankstart -> RightAsLeft = LeftAsRightV;
    _ -> {_,_,RightAsLeft} = PrevCol
  end,
  {_,Col,_} = CurrCol,
  case NextCol of
    blankend -> LeftAsRight = RightAsRightV;
    _ -> {LeftAsRight,_,_} = NextCol
  end,
  {RightAsLeft,Col,LeftAsRight}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda pomocnicza, zwraca indeks obiektu na liscie.
%% ---------------------------------------------------------------------------------------------------------------------
-spec indexOf(any(),list()) -> integer().
indexOf(Elem,List) -> indexOf(Elem,List, 1).
indexOf(_, [], _) -> {err, noelement};
indexOf(Elem, List, Index) ->
	case lists:nth(Index, List) == Elem of
		true -> Index;
		false -> indexOf(Elem,List,Index+1)
	end.

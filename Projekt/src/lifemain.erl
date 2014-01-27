%% @author Szymon Konicki
%% @doc Glowny modul aplikacji.
%% @end
-module(lifemain).
-export([nodeBenchmark/1,
  test_time/2,
  writeFinalColumns/3,
  iterateLocal/8,
  prepareColumnTuples/6,
  borderTuplesToColumnTriples/1,
  columnTripleToTuple/3,
  indexOf/2,
  iterateSingleMachine/9,
  calculateSingleColumn/3,
  genDoc/0]).


-type column() :: integer().
-type columns() :: [column()].
-type border() :: integer().
-type columnTuple() :: {border(), column(), border()}.
-type columnTuples() :: [columnTuple()].
-type columnTriple() :: {columnTuple(),columnTuple(),columnTuple()}.
-type columnTriples() :: [columnTriple()].


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda benchmarkujaca wezly.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec nodeBenchmark([node()]) -> ok.
nodeBenchmark(Nodes) ->
  Size = lifeio:getSize('/fff.gz'),
  CNodes = case length(Nodes) of 0->0; 1->1; 2->2; 3->2; 4->4; 5->4; 6->4; 7->4; 8->8; 9->8; 10->8 end,
%ProcList = lists:sublist([1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384], Size-6, 6),
  ProcList = [2,4,8,16,32,64,128,256,512],
  io:format("Size: ~b (~b), Proc: ~w~n", [Size, round(math:pow(2,Size)), ProcList]),
  iterateForProcesses(CNodes, ProcList, Nodes).

iterateForProcesses(_, [], _) -> ok;
iterateForProcesses(CNodes, [CProc|Rest], Nodes) ->
  iterateForNodes(CNodes, CProc, Nodes),
  iterateForProcesses(CNodes, Rest, Nodes).

iterateForNodes(0, CColumns, _) ->
  Configuration = {0, CColumns, []},
  test_time(2,false,Configuration),
  ok;
iterateForNodes(CNodes,CColumns, Nodes) ->
  SubNodes = lists:sublist(Nodes, CNodes),
  Configuration = {CNodes, CColumns, SubNodes},
  test_time(2,false,Configuration),
  iterateForNodes(CNodes div 2, CColumns, Nodes).


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda wykonujaca zadana ilosc iteracji korzystajac z najepszej konfiguracji.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec test_time(integer(), atom()) -> ok.
test_time(IterationCount, WriteFinalBoard)->
  Size = lifeio:getSize('/fff.gz'),
  test_time(IterationCount, WriteFinalBoard, lifeconc:getBestConfiguration(Size)).

test_time(IterationCount, WriteFinalBoard, Configuration)->
	{NodesCount, ColumnsCount, Nodes} = Configuration,
	{BoardSize, Columns} = lifeio:readDataToColumns('/fff.gz', ColumnsCount),
	ColumnWidth = BoardSize div ColumnsCount,
	{InnerConstant, LeftConstant, RightConstant, Zero} = lifelogic:createConstants(ColumnWidth, BoardSize),
	{IterationTime, Result} = case NodesCount of
		0 -> lifemain:iterateSingleMachine(Columns, ColumnWidth, BoardSize, Zero, Zero, LeftConstant, RightConstant, InnerConstant, IterationCount);
		_ -> lifeconc:mainController(Nodes, Columns, ColumnWidth, BoardSize, ColumnsCount, {InnerConstant, LeftConstant, RightConstant, Zero}, IterationCount)
	end,
	io:format("Time ~p~n", [IterationTime]),
	case WriteFinalBoard of 
		true ->
			io:format("Zapisuje tablice do pliku~n",[]),
			writeFinalColumns(Result, ColumnWidth, BoardSize),
			io:format("Tablica zapisana~n",[]);
		false -> ok
	end,
  ok.

%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda wywolujaca metode iterate zadana ilosc razy na biezacej maszynie. Zwraca kolumny po iteracji oraz czas
%%      iteracji.
%% ---------------------------------------------------------------------------------------------------------------------
-spec iterateSingleMachine(columns(), integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()) -> {integer(), columns()}.
iterateSingleMachine(Columns,ColumnWidth,Height, LeftAsRight, RightAsRight, LeftConstant, RightConstant, InnerBoardConst, IterationCounter) ->
  io:format("Nodes: 0, Cols: ~b, Iterations: ~b -> ",[length(Columns), IterationCounter]),
  Begin = now(),
  Result = lists:foldl(fun(_, Acc) ->
    lifemain:iterateLocal(Acc, ColumnWidth,Height,LeftAsRight, RightAsRight, LeftConstant, RightConstant,InnerBoardConst) end,
    Columns, lists:seq(1, IterationCounter)),
  End = now(),
  {timer:now_diff(End, Begin),Result}.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda zapisuje do pliku podane kolumny jako jedna tablice.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec writeFinalColumns(columns(), integer(), integer()) -> ok.
writeFinalColumns(Columns, ColumnWidth, Height)->
	Inners = lists:map(fun(Elem) -> lifelogic:getInnerBoard(Elem, ColumnWidth+2, Height+2) end, Columns),
	Glued = lifelogic:glue(Inners, ColumnWidth, Height),
	lifeio:writeBoardToFile(Glued, Height).


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda iterujaca na lokalnej maszynie. Z podanych kolumn tworzy krotki kolumn, ktore nastepnie rozsyla do
%%      Dostaje liste krotek kolumn do przetworzenia na maszynie, uzywajac metody rpc:pmap, przelicza dla kazdej kolumny
%%      nastepny stan i zwraca liste przeliczonych kolumn w tej samej kolejnosci.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec iterateLocal(columns(), integer(), integer(),integer(), integer(), integer(), integer(), integer()) -> columns().
iterateLocal(Columns,ColumnWidth,Height, LeftAsRight, RightAsRight, LeftConstant, RightConstant, InnerBoardConst) ->
  ColumnTuples = prepareColumnTuples(Columns,ColumnWidth,LeftAsRight, RightAsRight,LeftConstant, RightConstant),
  rpc:pmap({lifemain,calculateSingleColumn},[Height, InnerBoardConst],ColumnTuples).


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przeliczajaca pojedyncza kolumne. Na podstawie krotki kolumny tworzona jest kolumna rozszerzona
%%      o krawedzie poprzedniej i nastepnej kolumny, przeliczane sa stany wszystkich komorek kolumny, a nastepnie
%%      kolumna jest obcinana ze zbednych elementow i zwracana.
%% ---------------------------------------------------------------------------------------------------------------------
-spec calculateSingleColumn(columnTuple(), integer(), integer()) -> column().
calculateSingleColumn(ColumnTuple, ColumnWidth, InnerBoardConst) ->
  ColumnWithBorders = lifelogic:setBorders(InnerBoardConst, ColumnTuple),
  Next = lifelogic:next(ColumnWithBorders, ColumnWidth+2),
  Next.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda przygotowuje kolumny do przetwarzania. W jej wyniku powstaje lista krotek o postaci: {prawa krawedz
%%      poprzedniej kolumny (lub zero, jesli pierwsza kolumna), dana kolumna, lewa krawedz nastepnej kolumny (lub zero,
%%      jesli ostatnia kolumna)}. Takie struktury danych umozliwiaja niezalezne przeliczanie poszczegolnych kolumn
%%      w rozproszeniu.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec prepareColumnTuples(columns(), integer(), bitstring(), bitstring(), integer(), integer()) -> columnTuples().
prepareColumnTuples(Columns,ColumnWidth, LeftAsRight, RightAsRight, LeftConstant, RightConstant) ->
  BorderTuples = lists:map(fun(Column) -> lifelogic:getBorders(Column, LeftConstant, RightConstant, ColumnWidth) end, Columns),
  ColumnTriplets = borderTuplesToColumnTriples(BorderTuples),
  FinalColumnTuples = lists:map(fun(ColumnTriple) ->
    columnTripleToTuple(ColumnTriple, LeftAsRight, RightAsRight) end,
    ColumnTriplets),
  FinalColumnTuples.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda tworzaca trojke kolumn. Trojka kolumn to krotka zawierajaca 3 krotki kolumn - poprzedniej, biezacej oraz
%%      nastepnej kolumny. Metoda jest jednym z etapow tworzenia krotek kolumn - wygodnych do przetwarzania struktur.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec borderTuplesToColumnTriples(columnTuples()) -> columnTriples().
borderTuplesToColumnTriples(BorderTuples) ->
  ExtendedBorderTuples = [blankstart] ++ BorderTuples ++ [blankend],
  ColumnTriplets = lists:map(fun(BorderTuple) ->
    ListOfThree = lists:sublist(ExtendedBorderTuples, indexOf(BorderTuple,ExtendedBorderTuples), 3),
    list_to_tuple(ListOfThree) end,
    ExtendedBorderTuples),
  TrimmedColumnTriplets = lists:sublist(ColumnTriplets, 1, length(ExtendedBorderTuples)-2),
  TrimmedColumnTriplets.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda tworzaca krotke kolumny. Krotka kolumny to krotka zawieracaca kolejno: prawa krawedz poprzedniej kolumny
%%      (lub zera, jesli pierwsza kolumna), dana kolumne, lewa krawedz nastepnej kolumny (lub zera, jesli ostatnia
%%%     kolumna). Metoda jest jednym z etapow tworzenia krotek kolumn - wygodnych do przetwarzania struktur.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
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


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda pomocnicza, zwraca indeks obiektu na liscie.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec indexOf(any(),list()) -> integer().
indexOf(Elem,List) -> indexOf(Elem,List, 1).
indexOf(_, [], _) -> {err, noelement};
indexOf(Elem, List, Index) ->
	case lists:nth(Index, List) == Elem of
		true -> Index;
		false -> indexOf(Elem,List,Index+1)
	end.

%-----------------------------------------------------------------------------------------------------------------------
%% @doc Metoda pomocnicza, generuje dokumentacje dla wszystkich modulow.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
genDoc() ->
  edoc:files(['lifemain.erl','lifeconc.erl','lifelogic.erl'],[{dir,docs}]).
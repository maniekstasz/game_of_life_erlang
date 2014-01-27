%% @author Szymon Konicki
%% @doc Modul odpowiedzialny za rozproszenie obliczen.
%%      Zarzadza procesami rozproszonymi po
%%      wszystkich dostepnych wezlach, zajmuje sie
%%      transferem danych i inicjalizowaniem
-module(lifeconc).
-export([
  mainController/7,
  nodeSupervise/7,
  getBestConfiguration/1
]).


-type modules() :: [module()].
-type nodes() :: [node()].
-type func() :: atom().
-type args () :: [any()].
-type pkey() :: pid().
-type column() :: integer().
-type columns() :: [column()].
-type nodeTuple() :: {integer(), node(), pid()}.

%% @doc Funkcja zwraca najbardziej korzystną konfigurację {liczbę wezłów(0 gdy ma się wykonać lokalnie), Liczbę wszystkich kolumn, węzły}
%%      dla danego rozmiaru planszy
-spec getBestConfiguration(integer()) -> {integer(), integer(), nodes()}.
getBestConfiguration(Size) ->
  Nodes = net_adm:world(),
  NodesCount = case length(Nodes) of 0->0; 1->0; 2->2; 3->2; 4->4; 5->4; 6->4; 7->4; _->8 end,
  case Size of
    8 -> %256
      CNodes = 0,
      CProc = 4;
    9 -> %1024
    CNodes = 0,
    CProc = 8;
    10 -> %1024
      CNodes = 0,
      CProc = 16;
    11 -> %2048
      CNodes = 2,
      CProc = 16;
    12 -> %4096
      CNodes = 0,
      CProc = 16;
    13 -> %8192
      CNodes = 0,
      CProc = 16;
    14 -> %16384
      CNodes = 0,
      CProc = 16
  end,
	{CNodes, CProc, lists:sublist(Nodes, CNodes)}.
	
%% @doc Funkcja ta to kontroler wezlow: uruchamia, synchronizuje i kończy procesy na wezlach.
-spec mainController(nodes(), columns(), integer(), integer(), integer(), integer(), integer()) -> {integer(),columns() }.
mainController(Nodes, Columns, ColumnWidth, Height, ColumnsCount,Constants, Iteration) ->
	{_,_,_,Zero} = Constants,
	NodeBorderTuples = initializeNodesSupervisors(Nodes, Columns, ColumnWidth, Height, ColumnsCount, Constants),
	io:format("Remote (~b nodes); Processes: ~b, Iterations: ~b -> ",[length(Nodes),ColumnsCount,Iteration]),
	Begin = now(),
	NodeTuples = nodeNext(NodeBorderTuples, Iteration, Zero),
	End = now(),
	ResultedColumns = callFinishOnNodes(NodeTuples),
	Result = getFinalColumns(ResultedColumns, ColumnWidth, Height),
	{timer:now_diff(End, Begin), Result}.		

	
%% @doc Funkcja kontroluje procesy na danym wezle. Komunikuje sie z mainController.
-spec nodeSupervise(columns(), integer(), integer(), integer(), {integer(), integer(), integer(), integer()}, node(), pid()) -> ok.
nodeSupervise(Columns, ColumnWidth, Height, ColumnsCount, {InnerConstant, LeftConstant, RightConstant, Zero}, ParentNode, NodeNumber) ->
	receive
		{Listener, RightAsRight, LeftAsRight} -> 
			NewColumns = lifemain:iterate(Columns, ColumnWidth,Height, ColumnsCount,LeftAsRight, RightAsRight, LeftConstant, RightConstant,InnerConstant),
			NodeBordersTuple = getNodeBorders(NewColumns, {NodeNumber, node(), self()}, LeftConstant, RightConstant, ColumnWidth, (ColumnWidth+2)*(Height+2)),
			Listener ! NodeBordersTuple,
			nodeSupervise(NewColumns, ColumnWidth, Height, ColumnsCount, {InnerConstant,LeftConstant, RightConstant,Zero}, ParentNode, NodeNumber);
		{finish, Listener} ->
			Listener ! {finish, Columns, NodeNumber}
	end.


%% @doc Funkcja zwraca krotke {lewa krawedx wezla, dane wezla, prawa krawedz wezla}
-spec getNodeBorders(columns(),nodeTuple(), integer(), integer(), integer(), integer()) -> {integer(),nodeTuple(), integer() }.
getNodeBorders(Columns, NodeTuple, LeftConstant, RightConstant, ColumnWidth, BigSize) ->
	[First| _] = Columns,
	Last = lists:last(Columns),
	LeftBorder = lifelogic:getLeftAsRight(First, LeftConstant, ColumnWidth),
	RightBorder = lifelogic:getRightAsLeft(Last, RightConstant, ColumnWidth),
	{LeftBorder, NodeTuple, RightBorder}.

%% @doc Funkcja wymienia krawedzie pomiedzy wezlami i wywoluje nastepna iteracje.
-spec nodeNext([nodeTuple()], integer(), integer()) -> [nodeTuple()].
nodeNext(NodeBorderTuples, 0, Zero) ->
	NodeBorderTuples;
nodeNext(NodeBorderTuples, Iteration, Zero) ->
  	NodeTriplets = lifemain:borderTuplesToColumnTriples(NodeBorderTuples),
 	FinalColumnTuples = lists:map(fun(NodeTriplet) ->
    lifemain:columnTripleToTuple(NodeTriplet, Zero, Zero) end,
    NodeTriplets),
 	lists:foreach(fun({LeftAsRight, {_, _, NodeProcess}, RightAsLeft}) ->
				NodeProcess ! {self(), RightAsLeft, LeftAsRight} end,
				 FinalColumnTuples),
	nodeListener(length(NodeBorderTuples),Zero,Iteration, []).
	


%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu wszystkich iteracji. Laczy ze soba zwracane przez wezly kolumny.
nodeListener(0, -1, Acc)->
	Acc;
nodeListener(N, Iteration, Acc) ->
	receive
		{finish, Columns, NodeNumber} ->
			nodeListener(N-1, Iteration, [{Columns, NodeNumber}]++Acc)
	end.

%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu danej iteracji. Kiedy odbierze wszystkie sortuje odpowiedzi wzgledem numerow wezlow,
%%		aby mozliwa byla wymiana krawedzi.
nodeListener(0,Zero,Iteration, Acc)->
	SortedNodeBorderTuples = lists:sort(fun({_,{NodeNumberA, _,_},_},{_,{NodeNumberB, _,_},_})-> NodeNumberA =< NodeNumberB end, Acc),
	nodeNext(SortedNodeBorderTuples, Iteration-1, Zero)	;
nodeListener(N,Zero, Iteration, Acc) ->
	receive
		NodeIterResult -> 
		nodeListener(N-1,Zero, Iteration, [NodeIterResult] ++ Acc)
	end.

%% @doc Funkcja dzieli i przesyla kolumny na wezly. Zwraca dane o wezlach wraz z ich krawedziami
-spec initializeNodesSupervisors(nodes(), columns(), integer(), integer(), integer(), {integer(),integer(),integer(),integer()}) -> {integer(), nodeTuple(), integer()}.
initializeNodesSupervisors(Nodes, Columns, ColumnWidth, Height, ColumnsCount, Constants) ->
	NodesCount = length(Nodes),
	ColumnsPerNode = ColumnsCount div NodesCount,
	{_,LeftConstant, RightConstant,_} = Constants,
	NodeTuples = lists:map(fun(Node) ->
		NodeNumber = lifemain:indexOf(Node,Nodes)-1,
		case NodeNumber of
	    0 -> NodeColumns = lists:sublist(Columns,1 , ColumnsPerNode);
	    _ -> NodeColumns = lists:sublist(Columns,NodeNumber*ColumnsPerNode +1, ColumnsPerNode)
	    end,
		NodeProcess = spawn(Node, lifeconc, nodeSupervise, [NodeColumns, ColumnWidth, Height, length(NodeColumns),Constants, node(), NodeNumber]),
		getNodeBorders(NodeColumns, {NodeNumber, Node, NodeProcess}, LeftConstant, RightConstant, ColumnWidth, (ColumnWidth+2)*(Height+2) )
	    end,
    Nodes),
	NodeTuples.


callFinishOnNodes(NodeTuples) -> 
	lists:foreach(fun({_, {_, Node, NodeProcess}, _}) ->
				NodeProcess ! {finish, self()} end, NodeTuples),
	nodeListener(length(NodeTuples), -1,[]).

	
getFinalColumns(Columns, ColumnWidth, Height)->
	Sorted = lists:sort(fun({_,NodeNumberA},{_,NodeNumberB})-> NodeNumberA >= NodeNumberB end, Columns),
	Result = lists:foldl(fun({Columns, _}, Acc) ->  Columns ++ Acc end, [], Sorted).

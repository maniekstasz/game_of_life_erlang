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


-type nodes() :: [node()].
-type column() :: integer().
-type columns() :: [column()].
-type nodeTuple() :: {integer(), node(), pid()}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja zwraca najbardziej korzystną konfigurację {liczbę wezłów (0 gdy ma się wykonać lokalnie), liczbę
%%      wszystkich kolumn, węzły} dla danego rozmiaru planszy
%% ---------------------------------------------------------------------------------------------------------------------
-spec getBestConfiguration(integer()) -> {integer(), integer(), nodes()}.
getBestConfiguration(Size) ->
  Nodes = net_adm:world(),
  io:format("Wezly odpowiadaja kolejno:~n~w~n", [lists:map(fun(X) -> net_adm:ping(X) end, Nodes)]),
  CNodes = case length(Nodes) of 1->1; 2->2; 3->2; 4->4; 5->4; 6->4; 7->4; 8->8; 9->8; 10->8 end,
  Columns = case Size of 8->8; 9->16; 10->32; 11->64; 12->65; 13->128; 14->128 end,
  {CNodes, Columns, lists:sublist(Nodes,CNodes)}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja ta to kontroler wezlow: uruchamia, synchronizuje i kończy procesy na wezlach.
%% ---------------------------------------------------------------------------------------------------------------------
-spec mainController(nodes(), columns(), integer(), integer(), integer(), integer(), integer()) ->
  {integer(), columns()}.
mainController(Nodes, Columns, ColumnWidth, Height, ColumnsCount,Constants, Iteration) ->
	{_,_,_,Zero} = Constants,
	NodeBorderTuples = initializeNodesSupervisors(Nodes, Columns, ColumnWidth, Height, ColumnsCount, Constants),
	io:format("Przystepujemy do iteracji. Od tej pory liczymy czas~n",[]),
	Begin = now(),
	NodeTuples = nodeNext(NodeBorderTuples, Iteration, Zero),
	End = now(),
	io:format("Iteracja zakonczona koniec pomiaru czasu ~n",[]),
	ResultedColumns = callFinishOnNodes(NodeTuples),
	Result = getFinalColumns(ResultedColumns),
	{timer:now_diff(End, Begin), Result}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja kontroluje procesy na danym wezle. Komunikuje sie z mainController.
%% ---------------------------------------------------------------------------------------------------------------------
-spec nodeSupervise(columns(), integer(), integer(), integer(), {integer(), integer(), integer(), integer()}, node(), pid()) -> ok.
nodeSupervise(Columns, ColumnWidth, Height, ColumnsCount, {InnerConstant, LeftConstant, RightConstant, Zero}, ParentNode, NodeNumber) ->
	receive
		{Listener, RightAsRight, LeftAsRight} -> 
			NewColumns = lifemain:iterate(Columns, ColumnWidth,Height,LeftAsRight, RightAsRight, LeftConstant, RightConstant,InnerConstant),
			NodeBordersTuple = getNodeBorders(NewColumns, {NodeNumber, node(), self()}, LeftConstant, RightConstant, ColumnWidth),
			Listener ! NodeBordersTuple,
			nodeSupervise(NewColumns, ColumnWidth, Height, ColumnsCount, {InnerConstant,LeftConstant, RightConstant,Zero}, ParentNode, NodeNumber);
		{finish, Listener} ->
			Listener ! {finish, Columns, NodeNumber}
	end.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja zwraca krotke {lewa krawedx wezla, dane wezla, prawa krawedz wezla}
%% ---------------------------------------------------------------------------------------------------------------------
-spec getNodeBorders(columns(),nodeTuple(), integer(), integer(), integer()) -> {integer(),nodeTuple(), integer() }.
getNodeBorders(Columns, NodeTuple, LeftConstant, RightConstant, ColumnWidth) ->
	[First| _] = Columns,
	Last = lists:last(Columns),
	LeftBorder = lifelogic:getLeftAsRight(First, LeftConstant, ColumnWidth),
	RightBorder = lifelogic:getRightAsLeft(Last, RightConstant, ColumnWidth),
	{LeftBorder, NodeTuple, RightBorder}.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja wymienia krawedzie pomiedzy wezlami i wywoluje nastepna iteracje.
%% ---------------------------------------------------------------------------------------------------------------------
-spec nodeNext([nodeTuple()], integer(), integer()) -> [nodeTuple()].
nodeNext(NodeBorderTuples, 0, _) -> NodeBorderTuples;
nodeNext(NodeBorderTuples, Iteration, Zero) ->
  	NodeTriplets = lifemain:borderTuplesToColumnTriples(NodeBorderTuples),
 	FinalColumnTuples = lists:map(fun(NodeTriplet) ->
    lifemain:columnTripleToTuple(NodeTriplet, Zero, Zero) end,
    NodeTriplets),
 	lists:foreach(fun({LeftAsRight, {_, _, NodeProcess}, RightAsLeft}) ->
				NodeProcess ! {self(), RightAsLeft, LeftAsRight} end,
				 FinalColumnTuples),
	nodeListener(length(NodeBorderTuples),Zero,Iteration, []).


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu wszystkich iteracji. Laczy ze soba zwracane przez wezly kolumny.
%% ---------------------------------------------------------------------------------------------------------------------
nodeListener(0, -1, Acc)->
	Acc;
nodeListener(N, Iteration, Acc) ->
	receive
		{finish, Columns, NodeNumber} ->
			nodeListener(N-1, Iteration, [{Columns, NodeNumber}]++Acc)
	end.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu danej iteracji. Kiedy odbierze wszystkie sortuje odpowiedzi
%%      wzgledem numerow wezlow, aby mozliwa byla wymiana krawedzi.
%% ---------------------------------------------------------------------------------------------------------------------
nodeListener(0,Zero,Iteration, Acc)->
	SortedNodeBorderTuples = lists:sort(fun({_,{NodeNumberA, _,_},_},{_,{NodeNumberB, _,_},_})-> NodeNumberA =< NodeNumberB end, Acc),
	nodeNext(SortedNodeBorderTuples, Iteration-1, Zero)	;
nodeListener(N,Zero, Iteration, Acc) ->
	receive
		NodeIterResult -> 
		nodeListener(N-1,Zero, Iteration, [NodeIterResult] ++ Acc)
	end.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja dzieli i przesyla kolumny na wezly. Zwraca dane o wezlach wraz z ich krawedziami
%% ---------------------------------------------------------------------------------------------------------------------
-spec initializeNodesSupervisors(nodes(), columns(), integer(), integer(), integer(), {integer(),integer(),integer(),integer()}) -> {integer(), nodeTuple(), integer()}.
initializeNodesSupervisors(Nodes, Columns, ColumnWidth, Height, ColumnsCount, Constants) ->
	NodesCount = length(Nodes),
	ColumnsPerNode = ColumnsCount div NodesCount,
	{_,LeftConstant, RightConstant,_} = Constants,
	io:format("Rozpoczynam przesylanie danych do ~p wezlow ~n", [NodesCount]),
	NodeTuples = lists:map(fun(Node) ->
		NodeNumber = lifemain:indexOf(Node,Nodes)-1,
		case NodeNumber of
	    0 -> NodeColumns = lists:sublist(Columns,1 , ColumnsPerNode);
	    _ -> NodeColumns = lists:sublist(Columns,NodeNumber*ColumnsPerNode +1, ColumnsPerNode)
	    end,
		io:format("~p ", [Node]),
		NodeProcess = spawn(Node, lifeconc, nodeSupervise, [NodeColumns, ColumnWidth, Height, length(NodeColumns),Constants, node(), NodeNumber]),
		getNodeBorders(NodeColumns, {NodeNumber, Node, NodeProcess}, LeftConstant, RightConstant, ColumnWidth)
	    end,
    Nodes),
	io:format("~nDane przeslane~n"),
	NodeTuples.


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja
%% ---------------------------------------------------------------------------------------------------------------------
callFinishOnNodes(NodeTuples) -> 
	lists:foreach(fun({_, {_, _, NodeProcess}, _}) ->
				NodeProcess ! {finish, self()} end, NodeTuples),
	nodeListener(length(NodeTuples), -1,[]).


%% ---------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja
%% ---------------------------------------------------------------------------------------------------------------------
getFinalColumns(Columns)->
	Sorted = lists:sort(fun({_,NodeNumberA},{_,NodeNumberB})-> NodeNumberA >= NodeNumberB end, Columns),
	Result = lists:foldl(fun({Cols, _}, Acc) ->  Cols ++ Acc end, [], Sorted),
  Result.

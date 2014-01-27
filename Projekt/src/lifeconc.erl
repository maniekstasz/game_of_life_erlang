%% @author Szymon Konicki
%% @doc Modul odpowiedzialny za rozproszenie obliczen. Zarzadza procesami rozproszonymi po wszystkich dostepnych
%%      wezlach, zajmuje sie transferem danych i inicjalizowaniem.
%% @end
-module(lifeconc).
-export([
  getBestConfiguration/1,
  mainController/7,
  nodeSupervise/7,
  getNodeBorders/5,
  nodeNext/3,
  nodeListener/3,
  nodeListener/4,
  initializeNodesSupervisors/6,
  callFinishOnNodes/1,
  getFinalColumns/1]).


%metoda wolana nie wprost
-compile([{nowarn_unused_function, [{ nodeSupervise, 7}]}]).


-type nodes() :: [node()].
-type column() :: integer().
-type columns() :: [column()].
-type nodeTuple() :: {integer(), node(), pid()}.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja zwraca najbardziej korzystna konfiguracje do wykonania zadania. Forma zwrotki {liczbe wezlow (0 gdy ma
%%      sie wykonać lokalnie), liczbe wszystkich kolumn, wezly} dla danego rozmiaru planszy
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec getBestConfiguration(integer()) -> {integer(), integer(), nodes()}.
getBestConfiguration(Size) ->
  Nodes = net_adm:world(),
  CNodes = case length(Nodes) of 0->0; 1->1; 2->2; 3->2; 4->4; 5->4; 6->4; 7->4; 8->8; 9->8; 10->8 end,
  Columns = case Size of 8->8; 9->16; 10->32; 11->64; 12->65; 13->128; 14->128 end,
  {CNodes, Columns, lists:sublist(Nodes,CNodes)}.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Kontroler wezlow. Uruchamia, synchronizuje i konczy procesy na wezlach.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec mainController(nodes(), columns(), integer(), integer(), integer(), integer(), integer()) ->
  {integer(), columns()}.
mainController(Nodes, Columns, ColumnWidth, Height, ColumnsCount,Constants, Iteration) ->
	{_,_,_,Zero} = Constants,
	NodeBorderTuples = initializeNodesSupervisors(Nodes, Columns, ColumnWidth, Height, ColumnsCount, Constants),
	io:format("Nodes: ~b, Cols: ~b, Iterations: ~b -> ",[length(Nodes), ColumnsCount, Iteration]),
	Begin = now(),
	NodeTuples = nodeNext(NodeBorderTuples, Iteration, Zero),
	End = now(),
	ResultedColumns = callFinishOnNodes(NodeTuples),
	Result = getFinalColumns(ResultedColumns),
	{timer:now_diff(End, Begin), Result}.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja kontroluje procesy na danym wezle. Komunikuje sie z mainController.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec nodeSupervise(columns(), integer(), integer(), integer(), {integer(), integer(), integer(), integer()}, node(), pid()) -> ok.
nodeSupervise(Columns, ColumnWidth, Height, ColumnsCount, {InnerConstant, LeftConstant, RightConstant, Zero}, ParentNode, NodeNumber) ->
	receive
		{Listener, RightAsRight, LeftAsRight} -> 
			NewColumns = lifemain:iterateLocal(Columns, ColumnWidth,Height,LeftAsRight, RightAsRight, LeftConstant, RightConstant,InnerConstant),
			NodeBordersTuple = getNodeBorders(NewColumns, {NodeNumber, node(), self()}, LeftConstant, RightConstant, ColumnWidth),
			Listener ! NodeBordersTuple,
			nodeSupervise(NewColumns, ColumnWidth, Height, ColumnsCount, {InnerConstant,LeftConstant, RightConstant,Zero}, ParentNode, NodeNumber);
		{finish, Listener} ->
			Listener ! {finish, Columns, NodeNumber}
	end.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja zwraca krotke {lewa krawedx wezla, dane wezla, prawa krawedz wezla}.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec getNodeBorders(columns(),nodeTuple(), integer(), integer(), integer()) -> {integer(),nodeTuple(), integer() }.
getNodeBorders(Columns, NodeTuple, LeftConstant, RightConstant, ColumnWidth) ->
	[First| _] = Columns,
	Last = lists:last(Columns),
	LeftBorder = lifelogic:getLeftAsRight(First, LeftConstant, ColumnWidth),
	RightBorder = lifelogic:getRightAsLeft(Last, RightConstant, ColumnWidth),
	{LeftBorder, NodeTuple, RightBorder}.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja wymienia krawedzie pomiedzy wezlami i wywoluje nastepna iteracje.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
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


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu wszystkich iteracji. Laczy ze soba zwracane przez wezly kolumny.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec nodeListener(integer(),integer(),any()) -> ok.
nodeListener(0, -1, Acc)->
	Acc;
nodeListener(N, Iteration, Acc) ->
	receive
		{finish, Columns, NodeNumber} ->
			nodeListener(N-1, Iteration, [{Columns, NodeNumber}]++Acc)
	end.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja czeka na odpowiedz wezlow o zakonczeniu danej iteracji. Kiedy odbierze wszystkie sortuje odpowiedzi
%%      wzgledem numerow wezlow, aby mozliwa byla wymiana krawedzi.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec nodeListener(integer(),integer(),integer(),any()) -> ok.
nodeListener(0,Zero,Iteration, Acc)->
	SortedNodeBorderTuples = lists:sort(fun({_,{NodeNumberA, _,_},_},{_,{NodeNumberB, _,_},_})-> NodeNumberA =< NodeNumberB end, Acc),
	nodeNext(SortedNodeBorderTuples, Iteration-1, Zero)	;
nodeListener(N,Zero, Iteration, Acc) ->
	receive
		NodeIterResult -> 
		nodeListener(N-1,Zero, Iteration, [NodeIterResult] ++ Acc)
	end.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja dzieli i przesyla kolumny na wezly. Zwraca dane o wezlach wraz z ich krawedziami
%% @end
%-----------------------------------------------------------------------------------------------------------------------
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
		getNodeBorders(NodeColumns, {NodeNumber, Node, NodeProcess}, LeftConstant, RightConstant, ColumnWidth)
	    end,
    Nodes),
	NodeTuples.


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja informuje wezly o zakonczeniu pracy.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec callFinishOnNodes([nodeTuple()]) -> ok.
callFinishOnNodes(NodeTuples) ->
	lists:foreach(fun({_, {_, _, NodeProcess}, _}) ->
				NodeProcess ! {finish, self()} end, NodeTuples),
	nodeListener(length(NodeTuples), -1,[]).


%-----------------------------------------------------------------------------------------------------------------------
%% @doc Funkcja skleja kolumny z wezlow w jedna tablice.
%% @end
%-----------------------------------------------------------------------------------------------------------------------
-spec getFinalColumns(columns()) -> integer().
getFinalColumns(Columns)->
	Sorted = lists:sort(fun({_,NodeNumberA},{_,NodeNumberB})-> NodeNumberA >= NodeNumberB end, Columns),
	Result = lists:foldl(fun({Cols, _}, Acc) ->  Cols ++ Acc end, [], Sorted),
  Result.

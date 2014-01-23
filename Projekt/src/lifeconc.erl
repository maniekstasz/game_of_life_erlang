%% @author Mateusz Jablonski
%% @doc Modul odpowiedzialny za rozproszenie obliczen.
%%      Zarzadza procesami rozproszonymi po
%%      wszystkich dostepnych wezlach, zajmuje sie
%%      transferem danych i inicjalizowaniem
-module(lifeconc).
-export([
  prepareNodes/0,
  loadModule/2,
  gatherResult/2,
  operateNode/3,
  columnProcess/4,
  supervise/4,
  synchronize/1,
  createNode/3,
  mainController/5,
  nodeSupervise/7
]).


-type modules() :: [module()].
-type nodes() :: [node()].
-type func() :: atom().
-type args () :: [any()].
-type pkey() :: pid().


%% @doc Metoda przygotowuje wezly do przetwarzania.
%%      Pobierana jest lista wezlow z modulu net_adm,
%%      nastepnie do wszystkich wezlow wgrywany jest
%%      skompilowany kod modulow 'lifeconc' oraz 'lifelofic',
%%      ktore sa wymagane do poprawnego dzialania programu
-spec prepareNodes() -> ok.
prepareNodes() ->
	Nodes = net_adm:world(),
	io:format("Mam wszystkie ~B wezly~n", [length(Nodes)]),
	loadModule(lifeconc, Nodes),
  load_module(lifelogic, Nodes),
	io:format("Moduly zaladowane~n"),
  Nodes.


%% @doc Metoda laduje podane moduly do wszystkich wezlow.
%%      Z pomoca rpc:call na wszystkich wezlach ladowane sa
%%      binarki modulow.
-spec loadModule(modules(), nodes()) -> ok.
loadModule(Module, Nodes) ->
    {_Mod, Bin, Filename} = code:get_object_code(Module),
    lists:map(fun(Node) -> rpc:call(Node, code, load_binary, [Module, Filename, Bin]) end, Nodes),
  ok.


%% @doc Metoda tworzy asynchroniczny proces.
%%      Na wezle uruchamiana jest metoda operateNode, ktora
%%      zwraca krotke {id_wezla:klucz_procesu}
-spec createNode(node(),pid(),{module(),func(),args()}) -> {node(),pkey()}.
createNode(Node,ParentPid,{M,F,A}) ->
  io:format("Tworzenie async procesu~n"),
  Key = rpc:async_call(Node, lifeconc, localTest, [Node, ParentPid]),
  io:format("Stworzony asynchroniczny proces na "),
  io:write({Node,Key}),
  io:format("~n"),
  {Node,Key}.

localTest(Node, Pid) ->
  Pid ! {ok, Node, lol},
  ok.

%% @doc Metoda wywolywana na zdalnym wezle.
%%      Powoduje uruchomienie M:F/A, a po uzyskaniu wyniku
%%      wysyla do swojego rodzica informacje, ze obliczenia zostaly
%%      wykonane i zwraca wynik obliczen.
-spec operateNode(node(),pid(),{module(),func(),args()}) -> any().
operateNode(Node, ParentPid, {M,F,A}) ->
  Res = erlang:apply(M, F, A),
  ParentPid ! {ok, Node},
  Res.


%% @doc Metoda nasluchuje odpowiedzi od wezlow.
%%      Gdy dostanie wiadomosc...
gatherResult(_, 0) -> ok;
gatherResult(NodeKeyList, NodesLeft) ->
	receive
		{ok, Node} ->
			{_, Key} = lists:keyfind(Node, 1, NodeKeyList),
			RemoteResult = rpc:yield(Key),
			io:format("- Dostalem od ~s klucz ~p; wartosc: ", [Node, Key]),
			io:write(RemoteResult),
			io:format("~n"),
      gatherResult(NodeKeyList, NodesLeft-1)
	after 15000 ->
			io:format("Timeout 10 seconds~n"),
			timeout
	end.%,
  %RemoteResult.



%% @doc Metoda kontroluje process na wezle
-spec columnProcess(bitstring(), integer(), integer(), pid()) -> ok.
columnProcess(Column,ColumnWidth, Height, Supervisor) ->
		lifelogic:next(Column, ColumnWidth+2, Height+2),
		Supervisor ! ok.
	
%% @doc Supervisor synchronizujacy processy
-spec supervise(integer(),integer(),integer(),any()) -> {ok}.
supervise(ColumnCount, ColumnWidth, Height, Columns) ->
	receive
		start ->
			Start = now(),
			[spawn(conc, columnProcess, [Column, ColumnWidth, Height, self()]) || Column <- Columns],
			synchronize(ColumnCount),
			Stop = now(),
			Diff = timer:now_diff(Stop, Start),
			io:format("~p~n", [Diff]),
			{ok}
	end.

synchronize(Counter) ->
	receive 
		ok -> case Counter >= 2 of 
		true -> 		synchronize(Counter -1);
		false -> {ok}
		end
	end. 

getNodeBorders(Columns, NodeTuple, LeftConstant, RightConstant, ColumnWidth, BigSize) ->
	[First| _] = Columns,
	Last = lists:last(Columns),
	LeftBorder = lifelogic:getLeftAsRight(First, LeftConstant, ColumnWidth),
	RightBorder = lifelogic:getRightAsLeft(Last, RightConstant, ColumnWidth),
	{LeftBorder, NodeTuple, RightBorder}.
	
nodeNext(NodeBorderTuples, 0, Zero) ->
	NodeBorderTuples;

nodeNext(NodeBorderTuples, Iteration, Zero) ->
	%Listener = spawn(lifeconc, nodeListener, [length(NodeBorderTuples),Zero, []]),
  	NodeTriplets = lifemain:borderTuplesToColumnTriples(NodeBorderTuples),
 	FinalColumnTuples = lists:map(fun(NodeTriplet) ->
    lifemain:columnTripleToTuple(NodeTriplet, Zero, Zero) end,
    NodeTriplets),
 	lists:foreach(fun({LeftAsRight, {_, _, NodeProcess}, RightAsLeft}) ->
				NodeProcess ! {self(), RightAsLeft, LeftAsRight} end,
				 FinalColumnTuples),
	nodeListener(length(NodeBorderTuples),Zero,Iteration, []).
	

nodeListener(0, _, -1, Acc)->
	Acc;
nodeListener(0,Zero,Iteration, Acc)->
	SortedNodeBorderTuples = lists:sort(fun({_,{NodeNumberA, _,_},_},{_,{NodeNumberB, _,_},_})-> NodeNumberA =< NodeNumberB end, Acc),
	nodeNext(SortedNodeBorderTuples, Iteration-1, Zero)	;

nodeListener(N,Zero, Iteration, Acc) ->
	receive
		{finish, Columns, NodeNumber} ->
			%lists:foreach(fun(Column) -> io:format("~p~n", [NodeNumber]), lifeio:writeBoard(Column, 4,18),io:format("~n",[]) end , Columns),
			nodeListener(N-1, Zero, Iteration, [{Columns, NodeNumber}]++Acc);
		NodeIterResult -> 
		nodeListener(N-1,Zero, Iteration, [NodeIterResult] ++ Acc)
		
	end.

initializeNodesSupervisors(Columns, ColumnWidth, Height, ColumnsCount, Constants) ->
	Nodes = net_adm:world(),
	
	NodesCount = length(Nodes),
	io:format("Wczytalismy~n", []),
	ColumnsPerNode = ColumnsCount div NodesCount,
	%ColumnsRest = ColumnsCount - ColumnsPerNode * NodesCount,
	
	{_,LeftConstant, RightConstant,_} = Constants,
	lists:map(fun(Node) ->
		NodeNumber = lifemain:indexOf(Node,Nodes)-1,
		case NodeNumber of
	    0 -> NodeColumns = lists:sublist(Columns,1 , ColumnsPerNode);
	    _ -> NodeColumns = lists:sublist(Columns,NodeNumber*ColumnsPerNode +1, ColumnsPerNode)
	    end,
		NodeProcess = spawn(Node, lifeconc, nodeSupervise, [NodeColumns, ColumnWidth, Height, length(NodeColumns),Constants, node(), NodeNumber]),
		getNodeBorders(NodeColumns, {NodeNumber, Node, NodeProcess}, LeftConstant, RightConstant, ColumnWidth, (ColumnWidth+2)*(Height+2) )
	    end,
    Nodes).

			

						
mainController(Columns, ColumnWidth, Height, ColumnsCount,Iteration) ->
	Constants = lifelogic:createConstants(ColumnWidth, Height),
	{_,_,_,Zero} = Constants,
	NodeBorderTuples = initializeNodesSupervisors(Columns, ColumnWidth, Height, ColumnsCount, Constants),
	Begin = now(),
	NodeBorderTuples = nodeNext(NodeBorderTuples, Iteration, Zero),
	End = now(),
	io:format("Czas iteracji bez wczytywania i zapisywania planszy~p~n", [timer:now_diff(End, Begin)]),
	Acc = lists:foreach(fun({_, {_, Node, NodeProcess}, _}) ->
				NodeProcess ! {finish, self()} end, NodeBorderTuples),
	nodeListener(length(NodeBorderTuples),  Zero,-1,[]),
	Sorted = lists:sort(fun({_,NodeNumberA},{_,NodeNumberB})-> NodeNumberA >= NodeNumberB end, Acc),
	Result = lists:foldl(fun({Columns, _}, Acc) ->  Columns ++ Acc end, [], Sorted),
	Inners = lists:map(fun(Elem) -> lifelogic:getInnerBoard(Elem, ColumnWidth+2, Height+2) end, Result),
	Glued = lifelogic:glue(Inners, ColumnWidth, Height),
	%lifeio:writeBoard(Glued, Height,Height),
	lifeio:writeBoardToFile(Glued, Height).
	
	

nodeSupervise(Columns, ColumnWidth, Height, ColumnsCount, {InnerConstant, LeftConstant, RightConstant, Zero}, ParentNode, NodeNumber) ->
	receive
		{Listener, RightAsRight, LeftAsRight} -> 
			NewColumns = lifemain:iterateLocal(Columns, ColumnWidth,Height, ColumnsCount,LeftAsRight, RightAsRight, LeftConstant, RightConstant,InnerConstant),
			NodeBordersTuple = getNodeBorders(NewColumns, {NodeNumber, node(), self()}, LeftConstant, RightConstant, ColumnWidth, (ColumnWidth+2)*(Height+2)),
		
			Listener ! NodeBordersTuple,
			nodeSupervise(NewColumns, ColumnWidth, Height, ColumnsCount, {InnerConstant,LeftConstant, RightConstant,Zero}, ParentNode, NodeNumber);
		{finish, Listener} ->
			Listener ! {finish, Columns, NodeNumber}
	end.

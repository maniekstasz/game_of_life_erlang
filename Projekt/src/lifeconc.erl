%% @author Mateusz Jablonski
%% @doc Modul odpowiedzialny za rozproszenie obliczen.
%%      Zarzadza procesami rozproszonymi po
%%      wszystkich dostepnych wezlach, zajmuje sie
%%      transferem danych i inicjalizowaniem
-module(lifeconc).
-export([
  prepareNodes/0,
  loadModule/2,
  operateNodes/4,
  listen/1,
  operateNode/3,
  testRemoteMethod/1,
  columnProcess/4,
  supervise/4,
  synchronize/1
]).


-type modules() :: [module()].
-type nodes() :: [node()].
-type func() :: atom().
-type args () :: [any()].
-type pkey() :: pid().


%% @doc Metoda przygotowuje wezly do przetwarzania.
%%      Pobierana jest lista wezlow z modulu net_adm,
%%      nastepnie do wszystkich wezlow wgrywany jest
%%      skompilowany kod modulow 'lifeconc' oraz 'lifelofic'
-spec prepareNodes() -> ok.
prepareNodes() ->
	Nodes = net_adm:world(),
	io:format("Have all nodes~n"),
	loadModule([lifeconc,lifelogic], Nodes),
	io:format("Modules loaded to all nodes~n"),
	NodeKeyList = operateNodes(Nodes, lifeconc,testRemoteMethod,[okeeeej]),
	io:format("All nodes operated~n"),
	listen(NodeKeyList),
  io:format("All nodes listened~n"),
	{ok, done}.


%% @doc Metoda laduje podane moduly do wszystkich wezlow.
-spec loadModule(modules(), nodes()) -> ok.
loadModule(Modules, Nodes) ->
  lists:map(fun(Module) ->
    {_Mod, Bin, Filename} = code:get_object_code(Module),
    lists:map(fun(Node) -> rpc:call(Node, code, load_binary, [Module, Filename, Bin]) end, Nodes)
  end, Modules),
  ok.


%% @doc Metoda tworzy asynchroniczne procesy.
%%      Na kazdym wezle uruchamiana jest metoda M:F/A.
%%      Metoda zwraca liste krotek w postaci
%%      identyfikator_wezla:klucz_procesu
-spec operateNodes(nodes(), module(), func(), args()) -> [{node(), pkey()}].
operateNodes(Nodes, M,F,A) ->
	Result = lists:map(fun(Node) -> {Node, rpc:async_call(Node, lifeconc, operateNode, [Node, self(), {M,F,A}])} end, Nodes),
	Result.


%% @doc Metoda nasluchuje odpowiedzi od wezlow.
%%      Gdy dostanie wiadomosc, wyswietla zwróconą przez
%%      dany wezel wartosc.
-spec listen([{node(),pkey()}]) -> ok.
listen(NodeKeyList) -> 
	receive
		{ok, Node} ->
			{_, Key} = lists:keyfind(Node, 1, NodeKeyList),
			RemoteResult = rpc:yield(Key),
			io:format("- Dostalem od ~s klucz ~p; wartosc: ", [Node, Key]),
			io:write(RemoteResult),
			io:format("~n"),
			listen(NodeKeyList)
	after 5000 ->
			io:format("Timeout~n"),
			timeout
	end.

%% @doc Metoda wywolywana na zdalnym wezle.
-spec operateNode(node(),pid(),{module(),func(),args()}) -> any().
operateNode(Node, ParentPid, {M,F,A}) ->
	Res = erlang:apply(M, F, [self()]),
	ParentPid ! {ok, Node},
	Res.

%testowa metoda dla operatoNode
testRemoteMethod(Var) ->
	timer:sleep(100),
	{localPid, Var}.
	
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
			[spawn(lifeconc, columnProcess, [Column, ColumnWidth, Height, self()]) || Column <- Columns],
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

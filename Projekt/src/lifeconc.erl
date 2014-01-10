%% @author Mateusz Jabłoński
%% @doc Moduł odpowiedzialny za rozproszenie obliczeń

-module(lifeconc).
-compile(export_all).

prepareNodes() ->
	Nodes = net_adm:world(),
	io:format("Have all nodes~n"),
	
	loadModule(lifeconc, Nodes),
	io:format("Module lifeconc loaded to all nodes~n"),
	
	loadModule(lifelogic, Nodes),
	io:format("Module lifelogic loaded to all nodes~n"),
	
	M = lifeconc,
	F = trolo,
	A = [okeeeej],
	
	NodeKeyList = operateNodes(Nodes, M,F,A),
	io:format("All nodes operated~n"),
	
	listen(NodeKeyList),
	{ok, done}.

% ładuje skompilowany kod modułu do podanych węzłów
% maszyny remote nie mają dostępu do binarek

loadModule(Module, Nodes) ->
	{_Mod, Bin, Filename} = code:get_object_code(Module),
	lists:map(fun(Node) -> rpc:call(Node, code, load_binary, [Module, Filename, Bin]) end, Nodes).

% utworzenie na każdym węźle asynchronicznego procesu
% zwraca słownik identyfikator_węzła:klucz_procesu

operateNodes(Nodes, M,F,A) ->
	Result = lists:map(fun(Node) -> {Node, rpc:async_call(Node, lifeconc, operateNode, [Node, self(), {M,F,A}])} end, Nodes),
	Result.

% nasłuchuje odpowiedzi z rozproszonych procesów
% gdy dostanie wiadomość, wyświetla zwróconą przez proces wartość

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

% metoda uruchomiona na pojedynczym węźle
% dzieli zadanie na lokalne procesy, następnie łączy i zwraca wynik
% powiadamia parent node'a o zakończeniu obliczeń

operateNode(Node, ParentPid, {M,F,A}) ->
	Res = erlang:apply(M, F, [self()]),
	ParentPid ! {ok, Node},
	Res.

trolo(Var) ->
	timer:sleep(100),
	{localPid, Var}.
	
%Controluje process na węźle	
columnProcess(Column,ColumnWidth, Height, Supervisor) ->
		lifelogic:next(Column, ColumnWidth+2, Height+2),
		Supervisor ! ok.
		
	
%Supervisor synchronizujacy processy
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

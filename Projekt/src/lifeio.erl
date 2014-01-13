-module(lifeio).
-export([prepareBoardToWrite/2,lifeRead/1,readData/2,
  lifeWrite/2,writeData/2,testWrite/1,testRead/1, readDataToColumns/2,writeBoard/3,readDataToBoard/1]).


%% Funkcja do wypisywania tablicy na ekran
writeBoard(_,_,0) -> ok;
writeBoard(Board, Width, Height) ->
	<<A:Width, Rest/bits>> = Board,
	Data = [B + 48 || <<B:1>> <= <<A:Width>>],
	io:fwrite("~s~n", [Data]),
	writeBoard(Rest,Width, Height-1).


%% otwarcie pliku do wczytywania
%% zwracany jest deskryptor pliku i rozmiar danych/planszy
lifeRead(FileName) ->
		{ok,FD} = file:open(FileName,[read,compressed]),
		case file:read(FD,1) of 
				{ok,[Data]} -> {FD,Data};
				eof -> io:format("~nKoniec~n",[]);
				{error,Reason} -> io:format("~s~n",[Reason])
		end.

%% odczytanie kolejnej porcji danych o ĹĽÄ…danym rozmiarze
readData(FD,Length) -> 
		case file:read(FD,Length) of 
				{ok,Data} -> Data;
				eof -> io:format("~nKoniec~n",[]);
				{error,Reason} -> io:format("~s~n",[Reason])
		end.

readParts(_,0,ColumnWidth,_,Acc) -> 
		Boards = lists:map(fun(Elem) -> 
					  <<Elem/bits, 0:1/unit:1, 0:ColumnWidth/unit:1,0:1/unit:1>> end, Acc),
		Boards;
readParts(FD, Total, ColumnWidth,Count,  Acc)->
	Boards = lists:map(fun(Elem) -> 
					  Data = readData(FD, ColumnWidth),
					  Board = << <<Bin:1>> || Bin <- Data >>,
					  <<Elem/bits, 0:1/unit:1, Board/bits,0:1/unit:1>> end, Acc),
	readParts(FD, Total - (ColumnWidth*Count),ColumnWidth,Count, Boards).
readParts(FD, Total, Width, Count) ->
	ColumnWidth = Width div Count + 2,
	Acc = [<<0:ColumnWidth/unit:1>> || _ <- lists:seq(1, Count)],
	readParts(FD, Total, ColumnWidth-2,Count, Acc).

readDataToBoard(FileName) ->
  {FD,Pow} = lifeRead(FileName),
  BoardSize = trunc(math:pow(2, Pow)),
  Data = readData(FD, BoardSize*BoardSize),
  Board = << <<Bin:1>> || Bin <- Data >>,
  {BoardSize, Board}.

%% Wczytuje tablicę z pliku
%% Dzieli ja na zadana liczbe column. Zwraca rozmiar calej tablicy liste kolumn i rozmiar kolumny(bez dwoch dodatkowych bitow)
readDataToColumns(FileName, ColumnsCount) ->
  {ok,Dir} = file:get_cwd(),
	{FD,Pow} = lifeRead(Dir ++ '/' ++ FileName),
	BoardSize = trunc(math:pow(2, Pow)),
	Boards = readParts(FD, BoardSize*BoardSize, BoardSize,ColumnsCount),
	{BoardSize, Boards}.

prepareBoardToWrite(0, _, Data) ->Data;
prepareBoardToWrite(BoardSize, Board, Data) ->
	<<Bit:1/integer,Rest/bits>> = Board,
	prepareBoardToWrite(BoardSize-1, Rest, [Bit + 48 | Data]).

prepareBoardToWrite(Board, BoardSize) ->
	prepareBoardToWrite(BoardSize, Board,[]).
	
	

%% otwarcie pliku do zapisu planszy o wskazanym rozmiarze
lifeWrite(FileName,Size)->
		{ok,FD} = file:open(FileName,[write,compressed]),
		file:write(FD,Size),
		{ok,FD}.

%% zapisanie kolejnej porcji danych
writeData(FD,Data) ->
		file:write(FD,Data).

%% procedura testowa zapisujÄ…ca losowÄ… plansze
%% o wskazanym rozmiarze
testWrite(Size) ->
		Len = trunc(math:pow(2,Size)),
		{ok,Dir} = file:get_cwd(),
		{ok,FD} = lifeWrite(Dir ++ '/fff.gz',8),
		file:write(FD,[Size]),
		feedData(FD,Len,Len),
		file:close(FD).


%% Trochę to funkcje zmieniłem, żeby zapisywała 0 i jedynki do pliku
feedData(_FD,0,_Len)-> ok;
feedData(FD,Count,Len) ->
		Data = [random:uniform(2)+47 || _ <- lists:seq(1, Len)],
		writeData(FD,Data),
		feedData(FD,Count-1,Len).


%% procedura testowa odczytujÄ…ca planszÄ™ z pliku
testRead(FileName) ->
		{FD,Size} = lifeRead(FileName),
		Len = trunc(math:pow(2,Size)),
		io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
		getData(FD,Len,Len),
		file:close(FD).


getData(_FD,_Len,0) -> ok;
getData(FD,Len,Count) ->
		readData(FD,Len),
		getData(FD,Len,Count-1).
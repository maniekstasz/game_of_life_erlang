%% @author Szymon Konicki
%% @doc Moduł odpowiedzialny za dzielenie planszy na kilka kolumn oraz za wyliczanie następnej postaci tablicy.


-module(lifelogic).

-compile(export_all).

%-export([test/0,createConstants/2,getBoardExtraTopAndBottom/2, getInnerBoard/3, next/3, split/3, getBorders/5, glue/3, setBorders/4]).

%%    ---------------- WAŻNE -----------------
%% Width, Height - rozmiary tablicy niepowiekszonej
%% BigWidth, BigHeight - rozmiary tablicy powiekszonej
%% BigSize - analogicznie



%% Przykładowe użycie funkcji które napisałem. Ta funkcja jest do skasowania.
%% Jest ona głupio napisana, powinno tytaj wszystko być na listach, ale tak lepiej widać jak działają funkcje.
test() ->
	{ok,Dir} = file:get_cwd(),
	{BoardSize, PreBoard} = lifeio:readDataToBoard(Dir ++ '/fff.gz'),
	Board = getBoardExtraTopAndBottom(PreBoard, BoardSize),
	ColumnCount = 4,
	ColumnWidth = BoardSize div ColumnCount,
	{InnerBoard, Left, Right, Zero} = createConstants(ColumnWidth, BoardSize),
	Columns= split(ColumnCount, BoardSize, Board),
	ColumnSize = (ColumnWidth + 2) *(BoardSize+2),
	Borders = lists:map(fun(Column) -> lifelogic:getBorders(Column, Left, Right, ColumnWidth, ColumnSize) end, Columns),
	BorderTuples = lifemain:prepareColumnData(Borders, Zero),
	
	ColumnsWithBorders = lists:map(fun(X) -> lifelogic:setBorders(InnerBoard, ColumnSize, X) end, BorderTuples),
	
	Nexts = lists:map(fun(Elem) -> next(Elem, ColumnWidth+2, BoardSize+2) end, ColumnsWithBorders),
	Inners = lists:map(fun(Elem) -> getInnerBoard(Elem, ColumnWidth+2, BoardSize+2) end, Nexts),
	Glued = glue(Inners, ColumnWidth, BoardSize),
	lifeio:writeBoard(Glued, BoardSize,BoardSize).

%% Tworze bitboardy potrzebne do obliczania i ustawiania granic. 
%% Należy je obliczyć raz dla każdego podziału i przechowywać je pomiędzy iteracjami
createConstants(Width, Height) ->
	BigSize = (Width+2)*(Height+2),
	Size = (Width+2)*Height,
	<<PreBigLeft:Size/integer-unit:1>> = << <<1:1/unit:1, 0:Width/unit:1, 0:1/unit:1>> || _ <- lists:seq(1,Height) >>,
	PreBigRight = PreBigLeft bsr (Width + 1),
	PreBorders = (PreBigLeft bor PreBigRight),
	PreInnerBoard = bnot (PreBigLeft bor PreBigRight),
	PreLeft = PreBigLeft bsr 1,
	PreRight = PreBigRight bsl 1,
	<<Zero:BigSize>> = <<0:BigSize>>,
	<<InnerBoard:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreInnerBoard:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	%<<Borders:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreBorders:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	%<<BigLeft:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreBigLeft:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	%<<BigRight:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreBigRight:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	<<Left:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreLeft:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	<<Right:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreRight:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	{InnerBoard, Left, Right, Zero}.



%% Pobiera lewą krawędź i zwraca ją jako prawą
getLeftAsRight(Board, LeftConstant, Width) ->
	Board band LeftConstant bsr Width.

%% Pobiera prawą krawędź i zwraca ją jako lewą
getRightAsLeft(Board, RightConstant, Width) ->
	Board band RightConstant bsl Width.

%% Zwraca obie krawędzie {lewą jako prawą, prawą jako lewą} patrz funkcje getLeftAsRight i getRightAsLeft
%% Co ważne fukcja zwaraca Integery nie bitstringi
getBorders(Board, LeftConstant, RightConstant, Width, BigSize) ->
	<<BoardAsInteger:BigSize>> = Board,
	{getLeftAsRight(BoardAsInteger, LeftConstant, Width), Board, getRightAsLeft(BoardAsInteger, RightConstant, Width)}.

%% Ustawia krawędzie, LeftBorder to krawędź którą mamy przypisać po lewej stronie wiec bedzie to RightAsLeft,
%% RightBorder analogicznie
setBorders(InnerConstant, BigSize, {LeftBorder, Board, RightBorder}) ->
	<<BoardAsInteger:BigSize>> = Board,
	Result = BoardAsInteger band InnerConstant bor LeftBorder bor RightBorder,
	<<Result:BigSize>>.

%% Skleja kolumny do siebie
glue(Boards, Width, Height) ->
	glue(Boards,Width, Height,0, <<0:0>>).
glue(Boards, Width,Height,Offset, Acc) ->
	RestSize = Width*Height - Width - Offset,
	D =  [<< <<B:Width>> || <<_:Offset, B:Width,_:RestSize>> <= A >> ||  A <- Boards],
	G = << <<A/bits>> || A <- D>>,
	NewAcc = <<Acc/bits, G/bits>>,
	case Offset < Width*Height of
	 	true -> glue(Boards, Width,Height, Offset+ Width, NewAcc);
		false -> NewAcc
	end.


%Dzeli podaną tablicę na zadaną ilość kolumn
%Board musi być podany jako bitstring
split(ColumnsCount, Width, Board) ->
	ColumnWidth = Width div ColumnsCount,
	split(Board,ColumnWidth, Width, 0, []).

split(Board, ColumnWidth, Width, Offset, Acc)->
	Rest = Width - Offset - ColumnWidth,
	case Offset =:= Width of
		true -> Acc;
		false -> split(Board,ColumnWidth, Width, Offset+ColumnWidth, Acc ++ [<< <<0:1/unit:1,Column:ColumnWidth/unit:1,0:1/unit:1>> ||<<_:Offset, Column:ColumnWidth,_:Rest>> <= Board>>] )
	end.

%% Zwraca tablicę bez krawędzi
getInnerBoard(Board,BigWidth, BigHeight) ->
	SmallerBoardSize = (BigWidth)*(BigHeight-2),
	<<_:BigWidth, SmallerBoard:SmallerBoardSize/bits, _:BigWidth>> = Board,
	BoardSize2 = BigWidth - 2,
	<< <<Line:BoardSize2>> || <<_:1, Line:BoardSize2, _:1>> <= SmallerBoard >>.

%Dostaje zwykłą tablice i dorzuca do niej zera na górze i na dole
getBoardExtraTopAndBottom(Board, Width) ->
	<<0:Width, Board/bits, 0:Width>>.

%Board musi byc podany jako bitstring
%Zwraca następny stan tablicy
next(BoardBitString, BigWidth, BigHeight) ->
	Size = BigWidth*BigHeight,
	<<Board:Size>> = BoardBitString,
	Neigh = [Board bsl 1,
		Board bsl 1 bsl BigWidth,
		Board bsl BigWidth,
		Board bsr 1,
		Board bsr 1 bsl BigWidth,
		Board bsr 1 bsr BigWidth,
		Board bsr BigWidth,
		Board bsl 1 bsr BigWidth],
		
	[L0, L1| Tail] = Neigh,		 
	S0 = bnot (L0 band L1),
	S1 = L0 bxor L1,
	S2 = L0 band L1,

	[FS3, FS2| Rest] = sum(Tail, [0, S2,S1,S0]),
	IntegerBoard = (bnot Board band FS3) bor (Board band FS3) bor (Board band FS2),
	<<IntegerBoard:Size>>.


sum([], Sums) -> Sums;

%Wylicza liczbę sąsiadów
sum([L|Tail], [S3, S2, S1, S0]) ->
	NL = bnot L,
	Sums = [((S3 band NL) bor (S2 band L)) , ((S2 band NL) bor (S1 band L)) , ((S1 band NL) bor (S0 band L)), (S0 band NL)],
	sum(Tail,Sums).


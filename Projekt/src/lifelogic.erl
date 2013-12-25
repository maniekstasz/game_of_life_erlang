%% @author Szymon Konicki
%% @doc Moduł odpowiedzialny za dzielenie planszy na kilka kolumn oraz za wyliczanie następnej postaci tablicy.


-module(lifelogic).

-export([getBoardExtraTopAndBottom/2, getInnerBoard/3, next/3, split/3, getLeftAsRight/2, getRightAsLeft/2]).

%%    ---------------- WAŻNE -----------------
%% Width, Height - rozmiary tablicy niepowiekszonej
%% BigWidth, BigHeight - rozmiary tablicy powiekszonej



%Pobiera z tablicy lewą krawędź i zapisują ją po prawej stronie, reszta bitów to 0
%Board musi byc podany jako bitstring
getLeftAsRight(Board, Width) ->
	Width1 = Width +1,
	<< <<0:Width1/unit:1,Left:1/unit:1>> ||  <<_:1, Left:1,_:Width>> <= Board >>.

%Analogicznie do getLeftAsRight
%Board musi byc podany jako bitstring
getRightAsLeft(Board, Width) ->
	Width1 = Width +1,
	<< <<Left:1/unit:1,0:Width1/unit:1>> ||  <<_:Width, Left:1, _:1>> <= Board >>.

%Dzeli podaną tablicę na zadaną ilość kolumn
%Board musi być podany jako bitstring
split(ColumnsCount, Width, Board) ->
	Lines = getLines(Width, Board),
	ColSize = Width div ColumnsCount,
	LinesWithColumns = divideIntoColumns(Lines, ColSize),
	lists:foldl(fun(Line, Acc) -> pair(Line, Acc,ColSize)  end, [], LinesWithColumns).

%Board musi byc podany jako bitstring
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

getInnerBoard(Board,BigWidth, BigHeight) ->
	SmallerBoardSize = (BigWidth)*(BigHeight-2),
	<<_:BigWidth, SmallerBoard:SmallerBoardSize/bits, _:BigWidth>> = Board,
	BoardSize2 = BigWidth - 2,
	<< <<Line:BoardSize2>> || <<_:1, Line:BoardSize2, _:1>> <= SmallerBoard >>.

%Dostaje zwykłą tablice i dorzuca do niej zera na górze i na dole
getBoardExtraTopAndBottom(Board, Width) ->
	<<0:Width, Board/bits, 0:Width>>.

sum([], Sums) -> Sums;

%Wylicza liczbę sąsiadów
sum([L|Tail], [S3, S2, S1, S0]) ->
	NL = bnot L,
	Sums = [((S3 band NL) bor (S2 band L)) , ((S2 band NL) bor (S1 band L)) , ((S1 band NL) bor (S0 band L)), (S0 band NL)],
	sum(Tail,Sums).

%Board musi być podany jako bitstring
getLines(BigWidth, Board) ->
	[ A ||<<A:BigWidth/bits>> <= Board].

%Dzieli linie i zwraca columny w każdej lini
%LineLength długiść lini bez dwóch bitów
divideIntoColumns(Lines, ColSize) ->
	lists:map(fun(Line) -> [B || <<B:ColSize/bits>> <= Line] end, Lines).



%Towrzy podzielone tablice. Każda z tablic to columna rozszerzona w każdym wierszu o 2 bity jeden z jednej drugi z drugiej strony
pair([],[],_,_,_, Acc) -> 
	Acc;
pair([HL|TL], BAcc, LB, LN,ColSize, Acc) ->
	ColSize1 = ColSize -1,
	<<_:ColSize1, B:1/bits>> = LB,
	<<N:1/bits, _/bits>> = LN,
	Add = <<B/bits, HL/bits, N/bits>>,
	case TL =:= [] of
		true -> Next = <<0:ColSize>>;
		false -> [Next|_] = TL
	end,
	case BAcc =:= [] of
		true -> pair(TL, [], HL,Next,ColSize, lists:append(Acc, [<<Add/bits>>]));
		false -> [AH|AT] = BAcc,
				pair(TL, AT, HL,Next,ColSize, lists:append(Acc, [<<AH/bits,Add/bits>>]))
	end.

pair(Line, B, ColSize) -> 
	[Next|_] = Line,
	pair(Line, B,<<0:ColSize>>,Next,ColSize, []).





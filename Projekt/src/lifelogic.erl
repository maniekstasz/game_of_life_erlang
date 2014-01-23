%% @author Szymon Konicki
%% @doc Modul logiczny.
%%      Zajmuje sie dzieleniem, laczeniem boardu,
%%      obliczaniem nastepnego stanu, wyliczaniem
%%      krawedzi - przetwarzaniem danych.
-module(lifelogic).
-export([
  createConstants/2,
  getLeftAsRight/3,
  getRightAsLeft/3,
  getBorders/5,
  setBorders/3,
  next/3,
  getInnerBoard/3,
  glue/3,
  getBoardExtraTopAndBottom/2]).

% === WAŻNE ===
% Width, Height - rozmiary tablicy niepowiekszonej
% BigWidth, BigHeight - rozmiary tablicy powiekszonej
% BigSize - analogicznie




%% @doc Metoda tworzaca dane statyczne.
%%      Do poprawnego przetwarzania danych wymagane
%%      sa stale, budowane na podstawie rozmiaru tablicy
-spec createConstants(integer(), integer()) -> {integer(),integer(),integer(),integer()}.
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
	<<Left:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreLeft:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	<<Right:BigSize>> = <<0:1/unit:1, 0:Width/unit:1, 0:1/unit:1, PreRight:Size/unit:1,0:1/unit:1, 0:Width/unit:1, 0:1/unit:1 >>,
	{InnerBoard, Left, Right, Zero}.

%% @doc Metoda przygotowujaca lewa krawedz sasiada.
%%      Pobiera lewa krawedz boarda i zwraca ja
%%      jako prawa
-spec getLeftAsRight(integer(), integer(), integer()) -> integer().
getLeftAsRight(Board, LeftConstant, Width) ->
	Board band LeftConstant bsr Width.


%% @doc Metoda przygotowujaca prawa krawedz sasiada.
%%      Pobiera prawa krawedz boarda i zwraca ja
%%      jako lewa
-spec getRightAsLeft(integer(), integer(), integer()) -> integer().
getRightAsLeft(Board, RightConstant, Width) ->
	Board band RightConstant bsl Width.



%% @doc Metoda przygotowujaca krawedzie.
%%      Zwraca obie krawedzie w formacie
%%      {lewa jako prawa, board, prawa jako lewa}.
%%      Co ważne fukcja zwraca Integery nie bitstringi
-spec getBorders(bitstring(), integer(), integer(), integer(), integer()) -> {integer(), bitstring(), integer()}.
getBorders(Board, LeftConstant, RightConstant, Width, BigSize) ->
	{getLeftAsRight(Board, LeftConstant, Width), Board, getRightAsLeft(Board, RightConstant, Width)}.


%% @doc Metoda ustawia krawedzie.
%%      LeftBorder to krawedz ktora mamy przypisac
%%      po lewej stronie wiec bedzie to RightAsLeft,
%%      RightBorder analogicznie
-spec setBorders(integer(), integer(), {integer(), bitstring(), integer()}) -> bitstring().
setBorders(InnerConstant, BigSize, {LeftBorder, Board, RightBorder}) ->
	Board band InnerConstant bor LeftBorder bor RightBorder.


%% @doc Metoda skleja kolumny do siebie
-spec glue(bitstring(), integer(), integer()) -> bitstring().
glue(Boards, Width, Height) -> glue(Boards,Width, Height,0, <<0:0>>).
glue(Boards, Width,Height,Offset, Acc) ->
	RestSize = Width*Height - Width - Offset,
	D =  [<< <<B:Width>> || <<_:Offset, B:Width,_:RestSize>> <= A >> ||  A <- Boards],
	G = << <<A/bits>> || A <- D>>,
	NewAcc = <<Acc/bits, G/bits>>,
	case Offset < Width*Height of
	 	true -> glue(Boards, Width,Height, Offset+ Width, NewAcc);
		false -> NewAcc
	end.



%% @doc Metoda zwraca tablice bez krawedzi
-spec getInnerBoard(bitstring(),integer(),integer()) -> bitstring().
getInnerBoard(BoardAsInteger,BigWidth, BigHeight) ->
	BigSize = BigWidth * BigHeight,
	Board = <<BoardAsInteger:BigSize>>,
	SmallerBoardSize = (BigWidth)*(BigHeight-2),
	<<_:BigWidth, SmallerBoard:SmallerBoardSize/bits, _:BigWidth>> = Board,
	BoardSize2 = BigWidth - 2,
	<< <<Line:BoardSize2>> || <<_:1, Line:BoardSize2, _:1>> <= SmallerBoard >>.

%Dostaje zwykla tablice i dorzuca do niej zera na gorze i na dole
getBoardExtraTopAndBottom(Board, Width) ->
	<<0:Width, Board/bits, 0:Width>>.

%% @doc Metoda wykonuje iteracje planszy wg reguly Conway'a.
%%      Board musi byc podany jako bitstring
%%      Zwraca nastepny stan tablicy
-spec next(bitstring(),integer(),integer()) -> bitstring().
next(Board, BigWidth, BigHeight) ->
	Neigh = [Board bsl 1,
		Board bsl 1 bsl BigWidth,
		Board bsl BigWidth,
		Board bsr 1 bsl BigWidth,
		Board bsr 1,
		Board bsr 1 bsr BigWidth,
		Board bsr BigWidth,
		Board bsl 1 bsr BigWidth],
		
	[L0, L1| Tail] = Neigh,	
	S2 = L0 band L1,
	S0 = bnot (L0 bor L1),
	S1 = L0 bxor L1,
	

	[FS3, FS2| Rest] = sum(Tail, [0, S2,S1,S0]),

	(bnot Board band FS3) bor (Board band FS3) bor (Board band FS2).


%% @doc Metoda wylicza liczbe sasiadow
-spec sum(any(),any()) -> any().
sum([], Sums) -> Sums;
sum([L|Tail], [S3, S2, S1, S0]) ->
	NL = bnot L,
	Sums = [((S3 band NL) bor (S2 band L)) , ((S2 band NL) bor (S1 band L)) , ((S1 band NL) bor (S0 band L)), (S0 band NL)],
	sum(Tail,Sums).


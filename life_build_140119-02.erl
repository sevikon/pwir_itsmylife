-module(life).
-export([saveArray/3, saveRandomArray/2, read/1, getRow/2, genRandomArray/1, start/0, divideArrayCheck/0, divideArray/3]).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wywolania
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% life:saveArray("lol",[[1,0,1,1],[1,1,0,1],[0,0,0,0],[1,1,1,1]],2).
% life:read("lol").
% life:genRandomArray(2).
% life:saveRandom("lol",2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mechanizm gry Life 
%% @author Mateusz Ścirka
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Rozmiar planszy        
%getSize() -> length(getList()).        

%Zwraca pojedynczy wiersz planszy
% @params Lista, Numer wiersza
getRow(List, Row) ->
        lists:sublist(List, Row, 1).
        
%Pobiera pierwsze trzy elementy listy
% @params Numer wiersza, Numer kolumny, Lista
firstThree(Row, Column, OldList) ->
        lists:sublist(lists:flatten(getRow(OldList, Row)), Column, 3).

%Zwraca sasiedztwo wraz z komorka srodkowa
% @params Numer wiersza, Numer kolumny, Lista
getNeighborhood(Row, Column, OldList) ->        
        firstThree(Row - 2, Column, OldList) ++
        firstThree(Row - 1, Column, OldList) ++
        firstThree(Row, Column, OldList).

%Zasady gry
% @params wartość komórki środkowej, suma sąsiadów
gameRules(Cell, Neighbors) ->
        if
                Cell == 0 -> if Neighbors == 3 -> 1; true -> 0 end;
                Cell == 1 -> if Neighbors == 3; Neighbors == 2 -> 1; true -> 0 end
        end.

%Oblicza nowy stan
% @params Numer wiersza w którym jesteśmy, numer kolumny -||-, lista poprzednich stanów, lista nowych stanów
calculate(IterY, _IterX, _OldList, NewList) when IterY == 2 ->
        NewList;
        
calculate(IterY, IterX, OldList, NewList) when IterX == 0 ->
        calculate(IterY - 1, length(OldList) - 2, OldList, NewList);
        
calculate(IterY, IterX, OldList, NewList) ->
        Neighborhood = getNeighborhood(IterY, IterX, OldList),
        [Cell | _] = lists:sublist(Neighborhood, 5, 1),
        Sum = lists:sum(Neighborhood),
        calculate(IterY, IterX - 1, OldList, NewList ++ [gameRules(Cell, Sum - Cell)]).
        
%Dodaje ogrodzenie z zer.
% @params Lista, jej szerokosc, miejsce dodania zer
addZeros(List, Size, Pos) ->
	case Pos of
		{top}  		 -> [First | _] = List, [lists:duplicate(length(First), 0)] ++ List;
		{bottom} 	 -> [First | _] = List, List ++ [lists:duplicate(length(First), 0)];
		{left} 		 -> addZeros(List, Size, 0, [], {left});
		{right}		 -> addZeros(List, Size, 0, [], {right});
		{topleft}	 -> addZeros(addZeros(List, Size, {left}), Size - 1, {top});
		{topright}	 -> addZeros(addZeros(List, Size, {right}), Size - 1, {top});
		{bottomleft} -> addZeros(addZeros(List, Size, {left}), Size - 1, {bottom});
		{bottomright}-> addZeros(addZeros(List, Size, {right}), Size - 1, {bottom});
		{around}	 -> addZeros(addZeros(addZeros(List, Size, 0, [], {around}), Size + 2, {top}), Size + 2, {bottom}) 
	end.
	
% @params Lista, Wielkosc (bez zer), Iteracja poczatkowa = 0, Lista z dodanymi zerami, 
addZeros(_List, Size, Iter, NewListWithZeros, _Pos) when Iter == Size ->
	NewListWithZeros;
	
addZeros(List, Size, Iter, NewListWithZeros, {left}) ->
	addZeros(List, Size, Iter + 1, NewListWithZeros ++ [lists:flatten([0] ++ lists:sublist(List, Iter + 1, 1))], {left});	
	
addZeros(List, Size, Iter, NewListWithZeros, {right}) ->
	addZeros(List, Size, Iter + 1, NewListWithZeros ++  [lists:flatten(lists:sublist(List, Iter + 1, 1) ++ [0])], {right});

addZeros(List, Size, Iter, NewListWithZeros, {around}) ->
	addZeros(List, Size, Iter + 1, NewListWithZeros ++  [[0] ++ lists:flatten(lists:sublist(List, Iter + 1, 1) ++ [0])], {around}).	
	
%iteracje (pętla) obliczajace nowy stan
%iteration(Iter, List) when Iter == 0 ->
%        List;
        
%iteration(Iter, List) ->
%        Calculated = lists:reverse(calculate(getSize(), getSize() - 2, List, [])),
%        iteration(Iter - 1, addZeros(0, Calculated, [], getSize() - 2)).
        

%Nasłuchiwanie serwera
% @params (narazie brak) Lista, Pozycja zer
listenServer() -> 
	slucham.
	%start(..). %tymczasowo bezsensowne parametry inicjalizacyjne.

%No to jazda
%@params Lista, Wymiar listy, Pozycja w ktorej dodac zera
start() -> 
		{Size, List} = read("fff"), %tymczasowo wczytuje z pliku, 
        WithZeros = addZeros(List, Size, {around}),
        lists:reverse(calculate(Size + 2, Size, WithZeros, [])).
	   
	   %addZeros(List, length(List), {top}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Niepotrzebne na razie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %% @about Pobiera element o konkretnym indeksie
% %% @author mscirka, sevikon
% %% @return string z zawartoscia pliku
% getAtIndex(Row, Column, OldList) ->
%         Znak = lists:sublist(lists:flatten(getRow(OldList, Row)), Column, 1),
%         whichChar(Znak).

% whichChar([0]) -> "jestem zerem";
% whichChar([1]) -> "jestem jedynka".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Modul do wczytywania z pliku i zapisywania
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%
%% Interfejs
%%%%%%%%%%%%%%%%%
%% read(FileName)
%% genRandomArray(Size)
%% saveRandomArray(FileName, Size)
%%%%%%%%%%%%%%%%%

%% @about Procedura testowa odczytująca planszę z pliku
%% @author ptm, sevikon
%% @param FileName - nazwa pliku
%% @return {Len,ArrayOfArray} Len - Dlugosc tablicy (2^size), ArrayOfArray - tablica
read(FileName) ->
    {FD,Size} = lifeRead(FileName),
    Len = trunc(math:pow(2,Size)),
    io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
    ArrayOfArray = getData(FD,Len,Len,[]),
    file:close(FD),
    {Len,ArrayOfArray}.

%% @about Otwarcie pliku do wczytywania.
%% @author ptm
%% @example lifeRead(tablica)
%% @param FileName - nazwa pliku
%% @return {FD,Data} FD - deskryptor pliku, Data - rozmiar danych/planszy
lifeRead(FileName) ->
    {ok,FD} = file:open(FileName,[read,compressed]),
    case file:read(FD,1) of
        {ok,[Data]} -> {FD,Data};
        eof -> io:format("~nKoniec~n",[]);
        {error,Reason} -> io:format("~s~n",[Reason])
    end.

%% @about Procedura testowa odczytująca planszę z pliku, zwraca tablice np [[1,0,1,1],[1,1,0,1],[0,0,0,0],[1,1,1,1]]
%% @author ptm, sevikon
%% @param FD - deskryptor pliku, Len - dlugosc tablicy, Count - ile razy ma sie jeszcze wykonac ,ArrayOfArray - tablica dobudowywana
%% @return ArrayOfArray - tablica
getData(_FD,_Len,0,ArrayOfArray) ->
    ArrayOfArray;
getData(FD,Len,Count,ArrayOfArray) ->
    SeviRead = readData(FD,Len),
    SeviRead2 = intoArrayOfChar(SeviRead),
    getData(FD,Len,Count-1,ArrayOfArray++[SeviRead2]).

%% @about Odczytanie kolejnej porcji danych o żądanym rozmiarze
%% @author ptm
%% @example readData(deskryptor,dlugosc)
%% @param FD - deskryptor pliku, Length - dlugosc tablicy
%% @return Data - wiersz danych
readData(FD,Length) ->
    case file:read(FD,Length) of
        {ok,Data} -> Data;
        eof -> io:format("~nKoniec~n",[]);
        {error,Reason} -> io:format("~s~n",[Reason])
    end.

%% @about Zamiana stringa np "111001" na liste [1,1,1,0,0,1]
%% @author sevikon
%% @example intoArrayOfChar("111001")
%% @param List - string np "111001"
%% @return Lista - Lista (np. [1,1,1,0,0,1])
intoArrayOfChar([]) -> 0; % in case the list is empty, return zero
intoArrayOfChar(List) -> intoArrayOfChar(List,[]).
intoArrayOfChar([], Lista) -> Lista; % when list empty, stop, report
intoArrayOfChar([Head|Tail], Lista) ->
case Head of
        49 -> intoArrayOfChar(Tail, Lista ++ [1]);
        48 -> intoArrayOfChar(Tail, Lista ++ [0]);
        _Other -> {Head}
end.

%% @about Procedura zapisuja losowa planszę do pliku
%% @author ptm, sevikon
%% @param FileName - nazwa pliku, Size - rozmiar tablicy - szerokosc planszy to 2^Size
%% @return ok if correct
saveRandomArray(FileName, Size) ->
        Array = genRandomArray(Size),
        Len = trunc(math:pow(2,Size)),
    {ok,FD} = lifeWrite(string:join([FileName,"gz"],"."),8),
    file:write(FD,[Size]),
    feedDataSave(FD,Len,Len,Array),
    file:close(FD).

%% @about Procedura zapisuja planszę do pliku
%% @author ptm, sevikon
%% @param FileName - nazwa pliku, Array - tablica postaci [[1,0,1,1],[1,1,0,1],[0,0,0,0],[1,1,1,1]], Size - rozmiar tablicy
%% @return ok if correct
saveArray(FileName,Array, Size) ->
        Len = trunc(math:pow(2,Size)),
    {ok,FD} = lifeWrite(string:join([FileName,"gz"],"."),8),
    file:write(FD,[Size]),
    feedDataSave(FD,Len,Len,Array),
    file:close(FD).

%% @about Otwarcie pliku do zapisu planszy o wskazanym rozmiarze
%% @author ptm, sevikon
%% @param FileName - nazwa pliku, Size - rozmiar
%% @return {ok,FD} ok - if correct, FD - deskryptor pliku do zapisywania
lifeWrite(FileName,Size)->
    {ok,FD} = file:open(FileName,[write,compressed]),
    file:write(FD,Size),
    {ok,FD}.

%% @about Zapisanie kolejnej paczki danych
%% @author ptm, sevikon
%% param FD - deskryptor pliku, Count - ile razy ma sie jeszcze wykonac, Len - dlugosc tablicy,Array - tablica zapisywana
%% @return ok if correct
feedDataSave(_FD,0,_Len,_Array)-> ok;
feedDataSave(FD,Count,Len,Array) ->
    Data = intoString(lists:flatten(lists:sublist(Array, Len-Count+1, 1))),
    writeData(FD,Data),
    feedDataSave(FD,Count-1,Len,Array).

%% @about Zamiana wiersza np [1,1,1,0,0,1] na stringa "111001"
%% @author sevikon
%% @example intoString([1,1,1,0,0,1])
%% @param List - Lista(wiersz) (np. [1,1,1,0,0,1])
%% @return wiersz "111001"
intoString([]) -> 0; % in case the list is empty, return zero
intoString(List) -> intoString(List,[]).
intoString([], Lista) -> Lista; % when list empty, stop, report
intoString([Head|Tail], Lista) ->
case Head of
        1 -> intoString(Tail, Lista ++ "1");
        0 -> intoString(Tail, Lista ++ "0");
        _Other -> {Head}
end.

%% @about Zapisanie kolejnej porcji danych
%% @author ptm
%% @param FD - deskryptor pliku do zapisywania, Data - dane
%% @return nothing
writeData(FD,Data) ->
    file:write(FD,Data).

%% @about Generowanie losowej planszy
%% @author sevikon
%% @param Size - rozmiar - szerokosc planszy to 2^Size
%% @return Array - nowo wygenerowana plansza
genRandomArray(Size)->
        Len = trunc(math:pow(2,Size)),
        genRandomArray(Len,Len,[]).

genRandomArray(0,_Len,Array) -> Array;
genRandomArray(Count,Len,Array) ->
    Row = [random:uniform(2)-1 || _ <- lists:seq(0, Len-1)],
    genRandomArray(Count-1,Len,Array++[Row]).

% @about Dzielenie tablicy na mniejsze
% @autor malgmaci	
% @params Lista, Liczba aktywnych nodów, Size - Szerokość głównej tablicy (Size x Size)
divideArray(List, ActiveNodes, Size) -> 
	MinCols = trunc(math:sqrt(length(List)) / ActiveNodes),
	%Length = trunc(length(List) / ActiveNodes),
	NumOfRows = Size div ActiveNodes,
	Rest = Size - (ActiveNodes * NumOfRows),
	Length = NumOfRows*Size,
	if 
		MinCols >= 2 -> divide(List,  [ [{left_top_right}] ++ [lists:sublist(List, 1, Size*(NumOfRows+1))]] , ActiveNodes, 1, Length, Size, NumOfRows, Rest);
		true -> List
	end.

%@params Lista, Nowa lista, Liczba aktywnych nodów, Obecna iteracja, Ilosc znakow pobieranych , Wielkość głównej tablicy, Ilość wersów dla małej tablicy, Wersy, które pozostały zostaną przyłączone do ostatniej tablicy
divide(_List, NewList, ActiveNodes, CurrentPart, _Length, _Size, _NumOfRows, _Rest) when ActiveNodes == CurrentPart ->
	NewList;

divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) when ActiveNodes == CurrentPart + 1 ->
	divide(List, NewList ++ [ [{left_bottom_right}] ++ [lists:sublist(List, (CurrentPart * Length+1)-Size, (NumOfRows*Size)+Size+Rest*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest);
	
divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) ->
	divide(List, NewList ++ [ [{left_right}] ++ [lists:sublist(List, (CurrentPart * Length+1), (NumOfRows*Size)+2*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest).
	

%funkcja pomocnicza do wywolania funkcji divideArray	
divideArrayCheck() ->
	List =[1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7], %Tablica 7 x 7
	ActiveNodes = 3,
	divideArray(List, ActiveNodes, 7). 	
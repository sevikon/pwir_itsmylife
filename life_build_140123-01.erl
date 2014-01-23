-module(life).
-export([comm/1, comm/2, loop/0, rpc/2, saveArray/3, saveRandomArray/2, read/1, genRandomArray/1]).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wywolania
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% life:saveArray("lol",[[1,0,1,1],[1,1,0,1],[0,0,0,0],[1,1,1,1]],2).
% life:read("lol").
% life:genRandomArray(2).
% life:saveRandom("lol",2)
% life:comm(Tablica,2) Tablica = {Size,List} np Tablica=read("fff")
% life:comm(2) domyslny plik "fff"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mechanizm gry Life
%% @author Mateusz Ścirka
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Jak z jednego wymiaru zrobic dwa :D  
%Jak z jednego wymiaru zrobic dwa :D  
toMultiArray(List, Width, Height) ->
    toMultiArray(List, Width, Height, [], 0).

toMultiArray(_List, _Width, Height, MultiList, It) when Height == It ->
    MultiList;
    
toMultiArray(List, Width, Height, MultiList, It) ->
    toMultiArray(List, Width, Height, MultiList ++ [lists:sublist(List, Width * It + 1, Width)], It + 1).
        
%Pobiera pierwsze trzy elementy listy
% @params Numer wiersza, Numer kolumny, Lista
firstThree(Row, Column, OldList) ->
        lists:sublist(lists:flatten(lists:sublist(OldList, Row, 1)), Column, 3).

%Zwraca sasiedztwo wraz z komorka srodkowa
% @params Numer wiersza, Numer kolumny, Lista
getNeighborhood(Y, X, List) ->
        firstThree(Y - 1, X - 1, List) ++
        firstThree(Y, X - 1, List) ++
        firstThree(Y + 1, X - 1, List).

%Zasady gry
% @params wartość komórki środkowej, suma sąsiadów
gameRules(Cell, Neighbors) ->
        if
                Cell == 0 -> if Neighbors == 3 -> 1; true -> 0 end;
                Cell == 1 -> if Neighbors == 3; Neighbors == 2 -> 1; true -> 0 end
        end.

%Oblicza nowy stan
% @params Numer wiersza w którym jesteśmy, numer kolumny -||-, lista poprzednich stanów, lista nowych stanów
calculate(List) ->
    [First | _ ] = List,
    Width = length(First),
    Height = length(List),      
    MultiArray = toMultiArray(lists:reverse(calculate(List, Height - 1, Width - 1, [])), Width - 2, Height - 2),
    Calculated = lists:sublist(List, 1, 1) 
            ++ addZeros(MultiArray, length(MultiArray), {left_right})
            ++ [lists:last(List)],
    Calculated.

calculate(List, IterY, IterX, Calculated) ->
        Neighborhood = getNeighborhood(IterY, IterX, List),
        [Cell | _] = lists:sublist(Neighborhood, 5, 1),
        Sum = lists:sum(Neighborhood),      
        
        if 
            IterY == 2, IterX == 2 -> Calculated ++ [gameRules(Cell, Sum - Cell)];
            true ->             
                if 
                    IterX == 2 -> 
                        [F | _] = List, 
                        calculate(List, IterY - 1, length(F) - 1, Calculated ++ [gameRules(Cell, Sum - Cell)]); 
                    true ->
                        calculate(List, IterY, IterX - 1, Calculated ++ [gameRules(Cell, Sum - Cell)])
                end
        end.    
        
%Dodaje ogrodzenie z zer.
% @params Lista, jej szerokosc, miejsce dodania zer
addZeros(List, Size, Pos) ->
        case Pos of
                {top}                  -> [First | _] = List, [lists:duplicate(length(First), 0)] ++ List;
                {bottom}          -> [First | _] = List, List ++ [lists:duplicate(length(First), 0)];
                {left}                  -> addZeros(List, Size, 0, [], {left});
                {right}                 -> addZeros(List, Size, 0, [], {right});
                {topleft}         -> addZeros(addZeros(List, Size, {left}), Size - 1, {top});
                {topright}         -> addZeros(addZeros(List, Size, {right}), Size - 1, {top});
                {bottomleft} -> addZeros(addZeros(List, Size, {left}), Size - 1, {bottom});
                {bottomright}-> addZeros(addZeros(List, Size, {right}), Size - 1, {bottom});
                {around}         -> addZeros(addZeros(addZeros(addZeros(List, Size, {top}), Size, {bottom}), Size + 2, {left}), Size + 2, {right});
                {left_top_right} -> addZeros(addZeros(addZeros(List, Size, {top}), Size + 1, {left}), Size + 1, {right});
                {left_bottom_right} -> addZeros(addZeros(addZeros(List, Size, {bottom}), Size + 1, {left}), Size + 1, {right});
                {left_right}        -> addZeros(addZeros(List, Size, {left}), Size, {right})
        end.
        
% @params Lista, Wielkosc (bez zer), Iteracja poczatkowa = 0, Lista z dodanymi zerami,
addZeros(_List, Size, Iter, NewListWithZeros, _Pos) when Iter == Size ->
        NewListWithZeros;
        
addZeros(List, Size, Iter, NewListWithZeros, {left}) ->
        addZeros(List, Size, Iter + 1, NewListWithZeros ++ [lists:flatten([0] ++ lists:sublist(List, Iter + 1, 1))], {left});        
        
addZeros(List, Size, Iter, NewListWithZeros, {right}) ->
        addZeros(List, Size, Iter + 1, NewListWithZeros ++ [lists:flatten(lists:sublist(List, Iter + 1, 1) ++ [0])], {right}).       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dzielenie tablicy
%% @author Malgorzata Maciurzynska
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%@params Lista, Nowa lista, Liczba aktywnych nodów, Obecna iteracja, Ilosc znakow pobieranych , Wielkość głównej tablicy, Ilość wersów dla małej tablicy, Wersy, które pozostały zostaną przyłączone do ostatniej tablicy
divide(_List, NewList, ActiveNodes, CurrentPart, _Length, _Size, _NumOfRows, _Rest) when ActiveNodes == CurrentPart ->
    NewList;

divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) when ActiveNodes == CurrentPart + 1 ->
    divide(List, NewList ++ [ [{left_bottom_right}] ++ [lists:sublist(List, (CurrentPart * Length+1)-Size, (NumOfRows*Size)+Size+Rest*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest);
    
divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) ->
    divide(List, NewList ++ [ [{left_right}] ++ [lists:sublist(List, (CurrentPart * Length+1)-Size, (NumOfRows*Size)+2*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SMIECI CHWILOWE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %No to jazda
% %@params Lista, Wymiar listy, Pozycja w ktorej dodac zera
% start() ->
%         {Size, List} = read("fff"), %tymczasowo wczytuje z pliku,
%         Divided = divideArray(lists:flatten(List), 2, Size),
%         [Head|Tail]= Divided,
%         [Param,Tablica]= Head,
%         NowaTablica = changeToListList(Tablica,Size),
%         WithZeros = addZeros(NowaTablica, length(NowaTablica), Param), 
% B=[
% [0,0,0,0,0,0,0,0,0,0],
% [0,0,1,1,1,0,1,1,1,0],
% [0,0,1,0,0,1,0,1,0,0],
% [0,0,0,0,1,0,0,0,0,0],
% [0,1,1,0,0,0,0,1,1,0],
% [0,1,0,0,1,0,0,0,0,0]
% ],
% Lol = calculate(B),
% Row = getLast(Lol),
% replaceLast(Lol,Row).

%funkcja pomocnicza do wywolania funkcji divideArray    
% divideArrayCheck() ->
%     % List =[1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7], %Tablica 7 x 7
%     List =[[1,1,1,1,1,1,1],[2,2,2,2,2,2,2],[3,3,3,3,3,3,3],[4,4,4,4,4,4,4],[5,5,5,5,5,5,5],[6,6,6,6,6,6,6],[7,7,7,7,7,7,7]],
%     List = genRandomArray(4),
%     ActiveNodes = 4,
%     divideArray( lists:flatten(List), ActiveNodes, 7).  

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Komunikacja
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @about Potrzebne zeby zwracac nie komunikaty tylko konkretne wartosci
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
    {Pid, Response} ->
        Response
    end.

%% @about Petla dla liczacego (niewolnika)
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return koniec (Status == stop wychodzi z petli)
loop() ->
    receive
    {From, {init, PidNadzorcy,Size}} -> 
        put(nadzorca, {ok, PidNadzorcy}), % ustawia nadzorce
        put(szerokoscTablicy, {ok, Size}), % ustawia nadzorce
        From ! {self(), ok},
        loop();
    {From, {wiersze, Index, Row}} -> 
        From ! {self(), ok}, %dzieki temu nie czekamy co zwroci i mozemy wolac innych niewolnikow
        {_,Tablica} = get(tablica),
        if 
            Index ==1 ->
                Nowa  = replaceLast(Tablica,Row);
            Index==2 ->
                [RowF,RowL]= Row,
                Nowa2  = replaceFirst(Tablica,RowF),
                Nowa  = replaceLast(Nowa2,RowL);
            true ->
                Nowa  = replaceFirst(Tablica,Row)
        end,   
        Policzona = calculate(Nowa), % LICZENIE
        put(tablica, {ok, Policzona}),
        FirstRow = getFirst(Policzona),
        LastRow = getLast(Policzona),
        {_,Nadzorca} = get(nadzorca),
        Me = self(),
        rpc(Nadzorca,{policzono,Me,[FirstRow,LastRow]}), % oglaszam ze obliczylem
        loop();
    {From, {zwroctablice}} -> 
        From ! {self(), ok}, %dzieki temu nie czekamy co zwroci i mozemy wolac innych niewolnikow
        {_,Tablica} = get(tablica),
        {_,Nadzorca} = get(nadzorca),
        Me = self(),
        rpc(Nadzorca,{wynik,Me,getMiddle(Tablica)}),
        loop();
    {From, {licz, Param, Tablica}} -> 
        From ! {self(), ok}, %dzieki temu nie czekamy co zwroci i mozemy wolac innych niewolnikow
        {_,Size} = get(szerokoscTablicy),
        NowaTablica = changeToListList(Tablica,Size),
        WithZeros= addZeros(NowaTablica, length(NowaTablica), Param), 
        Policzona = calculate(WithZeros),  % Liczenie
        put(tablica, {ok, Policzona}),
        FirstRow = getFirst(Policzona),
        LastRow = getLast(Policzona),
        {_,Nadzorca} = get(nadzorca),
        Me = self(),
        rpc(Nadzorca,{policzono,Me,[FirstRow,LastRow]}), % oglaszam ze obliczylem
        loop();
    {From, kill} ->
        From ! {self(), kill};
    {From, Other} ->
        From ! {self(), {error,Other}},
        loop()
    end.

%% @about Petla dla nadzorcy, wlasciwie to powinna byc obecna konsola
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return koniec (Status == stop wychodzi z petli)
loop2(ListaPidow,ActiveNodes,Size,Steps) ->    
    {_,Status} = get(status),
    if
        Status == start ->
            initNodes(ListaPidow,Size),
            {_,CalaTablica} = get(scalonatablica),
            Divided = divideArray(lists:flatten(CalaTablica), ActiveNodes, Size),
            nodesCalc(Divided,ListaPidow),
            io:fwrite("Kolejna iteracja~n"),
            put(status, {ok, wait}),
            loop2(ListaPidow,ActiveNodes,Size,Steps);
        Status == again ->
            put(iterator, {ok, length(ListaPidow)}), %na ile procesow trzeba czekac
            {_,Wiersze} = get(wiersze),
            nodeNewsRows(Wiersze,ListaPidow),
            put(status, {ok, wait}),
            put(wiersze, {ok, emptyArray(2*length(ListaPidow))}),
            loop2(ListaPidow,ActiveNodes,Size,Steps);
        Status == stop -> 
            killPids(ListaPidow),
            killPidsForDeath(ListaPidow),
            {_,Wyniki} = get(wyniki),
            Polaczona = joinArrays(Wyniki),
            Polaczona;
        Status == wait -> 
            receive
            {From, {wynik, Pid, WynikowaTablica}} -> 
                From ! {self(), ok},
                {_,Fragment} = get(Pid),
                % io:fwrite("Otrzymano ostateczny fragment: ~w~n", [Fragment]),
                {_,NowyIterator} = get(iterator),
                {_,Wyniki} = get(wyniki),
                put(wyniki, {ok, replaceRow(Wyniki,WynikowaTablica,Fragment)}),
                NowyIterator2= NowyIterator -1,
                put(iterator, {ok, NowyIterator2}),
                if
                    NowyIterator2 == 0 -> 
                        io:fwrite("Otrzymano wszystkie wyniki!"),
                        put(status, {ok, stop}),
                        loop2(ListaPidow,ActiveNodes,Size,Steps);
                    true -> 
                        loop2(ListaPidow,ActiveNodes,Size,Steps)
                end;        
            {From, {policzono, Pid, Tablica}} -> 
                From ! {self(), ok},
                {_,Fragment} = get(Pid),
                [TopRow,DownRow] = Tablica,
                % io:fwrite("Otrzymano wiersze z ~w fragmentu tablicy~n", [Fragment]),
                {_,NowyIterator} = get(iterator),
                {_,Wiersze} = get(wiersze),
                if
                    Fragment == 1 -> 
                        put(wiersze, {ok, replaceRow(Wiersze,DownRow,3)});
                    Fragment == length(ListaPidow) -> 
                        put(wiersze, {ok, replaceRow(Wiersze,TopRow,Fragment*2-2)});
                    true -> 
                        put(wiersze, {ok, replaceRow(replaceRow(Wiersze,TopRow,Fragment*2-2),DownRow,Fragment*2+1)})
                end,
                NowyIterator2= NowyIterator -1,
                put(iterator, {ok, NowyIterator2}),
                if
                    NowyIterator2 == 0 -> 
                        % io:fwrite("Otrzymano wszystkie z ~w!~n", [length(ListaPidow)]),
                        if
                            Steps == 1 -> 
                                % io:fwrite("Zakonczono wszystkie iteracje!"),
                                put(status, {ok, wait}),
                                put(iterator, {ok, length(ListaPidow)}), 
                                getTablesBack(ListaPidow), % prosba o zwrocenie wynikow
                                loop2(ListaPidow,ActiveNodes,Size,Steps-1);
                            true -> 
                                put(status, {ok, again}),
                                io:fwrite("Kolejna iteracja~n"),
                                loop2(ListaPidow,ActiveNodes,Size,Steps-1)
                        end;
                    true -> 
                        loop2(ListaPidow,ActiveNodes,Size,Steps)
                end;
            {From, Other} ->
                io:fwrite("Inne cos"),
                From ! {self(), {error,Other}},
                Other
            end;
        true -> 
            loop2(ListaPidow,ActiveNodes,Size,Steps)
    end.

%% @about Odpalenie przykladowy procesow, operacje na nich i czekanie na wyniki
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
% comm(TablicaReaded,Steps) ->
comm(Steps) ->
    comm(read("fff"),Steps).
comm(TablicaReaded,Steps) ->
    {Size, List} = TablicaReaded, %tymczasowo wczytuje z pliku,
    Cheats = 25,
    ActiveNodes = length(lists:sublist(nodes(),10))*Cheats,
    ListaPidow = createPids(Cheats), %utworzenie pidow
    put(iterator, {ok, length(ListaPidow)}), %na ile procesow trzeba czekac
    put(status, {ok, start}), %obecny status iteracji
    put(scalonatablica, {ok, List}), %cala tablica
    put(wiersze, {ok, emptyArray(2*length(ListaPidow))}), %cala tablica
    put(wyniki, {ok, emptyArray(length(ListaPidow))}),
    loop2(ListaPidow,ActiveNodes,Size,Steps). %czekanie na wyniki

%% @about Otworzenie nowych Pidow na nodeach
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
createPids(Times) -> 
    Nodes = lists:sublist(nodes(),10),
    Cheated  = cheatNodes(Nodes,Times),
    % ++lists:sublist(nodes(),10), %pobranie listy node'ow
    createPids(Cheated,[]). 
createPids([], ListaPidow) -> ListaPidow; 
createPids([Head|Tail], ListaPidow) ->
    %tu można zmienic liczbe procesow na nodzie !!!!!!!!!!!!
    Pid = spawn(Head,life,loop,[]), %utworzenie na nodach procesu, ktory czeka
    put(Pid, {ok, length(ListaPidow)+1}), %utworzenie mapy zeby miec dostep pozniej
    createPids(Tail, ListaPidow ++ [Pid]).

%% @about Wysyłanie do wszystkich procesów zadania obliczen wraz z kawalkami tablic (1 iteracja)
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
nodesCalc(Divided,ListaPidow) ->
    nodesCalc(Divided,ListaPidow, length(ListaPidow)).
nodesCalc([],_ListaPidow,Other) -> ok;
nodesCalc(Divided,_ListaPidow,0) -> ok;
nodesCalc(Divided,ListaPidow,Index) ->
    Pid = lists:nth(Index, ListaPidow),
    Head = lists:nth(Index, Divided),
    [Param,Tablica]= Head,
    rpc(Pid,{licz,Param,Tablica}),
    nodesCalc(Divided,ListaPidow,Index-1).


%% @about Wysyłanie do wszystkich procesów prosby o zwrocenie wynikow
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
getTablesBack(ListaPidow) -> 
    getTablesBack(ListaPidow, length(ListaPidow)).
getTablesBack(ListaPidow,0) -> ok;
getTablesBack(ListaPidow,Index) ->
    Pid = lists:nth(Index, ListaPidow),
    rpc(Pid,{zwroctablice}),
    getTablesBack(ListaPidow,Index-1).


%% @about Wysyłanie do wszystkich procesów nowych wierszy do podmiany i zmuszenie do iteracji
%% @author sevikon
%% @param Wiersze - tablica nowych wierszy, ListaPidow - Lista pidow na node'ach
%% @return ok if correct
nodeNewsRows(Wiersze,ListaPidow) ->
    nodeNewsRows(Wiersze,ListaPidow, length(ListaPidow)).
nodeNewsRows([],_ListaPidow,Other) -> ok;
nodeNewsRows(Wiersze,_ListaPidow,0) -> ok;
nodeNewsRows(Wiersze,ListaPidow,Index) ->
    Pid = lists:nth(Index, ListaPidow),
    Top = lists:nth(Index*2-1, Wiersze),
    Down = lists:nth(Index*2, Wiersze),
    if 
        Index==length(ListaPidow) ->
            rpc(Pid,{wiersze,3,Top});      
        Index==1 ->
            rpc(Pid,{wiersze,1,Down});
        true -> 
            rpc(Pid,{wiersze,2,[Top,Down]})
    end,
    nodeNewsRows(Wiersze,ListaPidow,Index-1).

%% @about Zabijanie pidow
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
killPids(ListaPidow) -> 
    killPids(ListaPidow, length(ListaPidow)).
killPids(_ListaPidow,0) -> ok;
killPids(ListaPidow,Index) ->
    Pid = lists:nth(Index, ListaPidow),
    rpc(Pid,kill),
    killPids(ListaPidow,Index-1).

%% @about Zabijanie pidow II
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
killPidsForDeath(ListaPidow) -> 
    killPidsForDeath(ListaPidow, length(ListaPidow)).
killPidsForDeath(_ListaPidow,0) -> ok;
killPidsForDeath(ListaPidow,Index) ->
    Pid = lists:nth(Index, ListaPidow),
    %% licz powinno byc inne
    exit(Pid,kill),
    killPidsForDeath(ListaPidow,Index-1).

%% @about Inicjowanie wszystkich pid'ow z listy
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
initNodes(ListaPidow,Size)->
    initNodes(ListaPidow,length(ListaPidow),Size).
initNodes(_ListaPidow,0,Size) -> ok;
initNodes(ListaPidow,Index,Size) ->
    Pid = lists:nth(Index, ListaPidow),
    rpc(Pid,{init,self(),Size}),
    initNodes(ListaPidow,Index-1,Size).

%% @about Zamiana z listy na liste list
%% @author sevikon
%% @param Lista-lista; Rozmiar - dlugosc wiersza
%% @return ListaWszystkich - lista list
changeToListList([],Rozmiar)->0;
changeToListList(Lista,Rozmiar)->changeToListList(Lista,Rozmiar,[],[],Rozmiar).
changeToListList([],Rozmiar,ListaWszystkich,Wiersz,Iterator) -> ListaWszystkich++[Wiersz]; % when list empty, stop, report
changeToListList(Lista,Rozmiar,ListaWszystkich,Wiersz,0) -> 
    changeToListList(Lista,Rozmiar,ListaWszystkich++[Wiersz],[],Rozmiar);
changeToListList([Head|Tail],Rozmiar,ListaWszystkich,Wiersz,Other) -> 
    changeToListList(Tail,Rozmiar,ListaWszystkich,Wiersz++[Head],Other-1).


%% @about Funkcje odpowiedzialne za wziecie odpowednich wierszy. NIE OSTATNI ANI PIERWSZY. Tylko pierwszy i ostatni policzony
%% @author sevikon
%% @param 
%% @return NewArray - srodkowa czesc tablicy
getLast(Array)-> lists:nth(length(Array)-1,Array).
getFirst(Array)-> lists:nth(2,Array).

%% @about Funkcje odpowiedzialne za podmiany wierszy w liscie
%% @author sevikon
%% @param Array - tablica (lista list), Row - wklejany wiersz, Index - ewentualna pozycja zamiany
%% @return NewArray - srodkowa czesc tablicy
replaceLast(Array,Row) -> lists:sublist(Array,length(Array)-1) ++ [Row].
replaceFirst(Array,Row) -> [Row]++lists:nthtail(1,Array).
replaceRow(Array,Row,Index) -> lists:sublist(Array,Index-1)++[Row]++lists:nthtail(Index,Array).

%% @about pusta lista jednowymiarowa (zera) o konkretnych wymiarach
%% @author sevikon
%% @param Rows - liczba elementow
%% @return Array -  pusta lista jednowymiarowa
emptyArray(Rows) -> emptyArray([],Rows). 
emptyArray(Array,0)->Array;
emptyArray(Array,Index)->
    NewArray=Array++[0],
    emptyArray(NewArray,Index-1).

%% @about Wziecie srodkowej tablicy z tablicy, tzn znika obramowka
%% @author sevikon
%% @param Array - tablica (lista list) z ktorej zostanie wyciety srodek
%% @return NewArray - srodkowa czesc tablicy
getMiddle(Array) ->
    ArrayNowy = lists:sublist(Array, 2, length(Array)-2),
    getMiddle(ArrayNowy,length(ArrayNowy),[]).
getMiddle(_,0,NewArray) ->NewArray;
getMiddle(Array,Index,NewArray) ->
    [Head|Tail] = Array,
    Wiersz = lists:sublist(Head, 2, length(Head)-2),
    getMiddle(Tail,Index-1,NewArray++[Wiersz]).

%% @about Scalanie kolejnych listy listy list :) w liste list
%% @author sevikon
%% @param ArrayOfArrays - lista listy list 
%% @return Nowa - lista list
joinArrays(ArrayOfArrays) ->
    joinArrays(ArrayOfArrays,[]).
joinArrays([],Nowa) -> Nowa;
joinArrays(ArrayOfArrays,Nowa) ->
    [Head|Tail] = ArrayOfArrays,
    joinArrays(Tail,Nowa++Head).

%% @about Potrzebne do robienia wiecej pidow na nodeach; kopiuje tablice Array Times razy
%% @author sevikon
%% @param Array - tablica kopiowana, Times - ile razy
%% @return NewArray - nowa lista
cheatNodes(Array,Times)->
    cheatNodes(Array,Times,[]).
cheatNodes(Array,0,NewArray)->NewArray; 
cheatNodes(Array,Times,NewArray)->
    cheatNodes(Array,Times-1,NewArray++Array).
-module(life).
-export([test/3, testTab/3, next/2, nextTab/2, comm/3, loop/0, rpc/2, saveArray/3, saveRandomArray/2, read/1, genRandomArray/1]).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wywolania
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% life:saveArray("nazwa",[[1,0,1,1],[1,1,0,1],[0,0,0,0],[1,1,1,1]],2). - zapisywanie konkretnej tablicy (2 arg) do pliku nazwa(1 arg) i rozmiarze (3arg)
% life:read("nazwa"). - wczytywanie konkretnej tablicy z pliku nazwa(1 arg)
% life:genRandomArray(2). - generowanie losowej tablicy o konkretnym rozmiarze(1 arg)
% life:saveRandom("nazwa",2) - zapisywanie losowej tablicy o konkretnym rozmiarze (2arg) do pliku nazwa(1 arg)


% life:next("nazwapliku",liczbaiteracji);
% life:nextTab(Tablica,liczbaiteracji); np Tablica=read("nazwa")
% life:test("nazwapliku",liczbaiteracji, liczbapowtorzen)
% life:testTab(Tablica,liczbaiteracji, liczbapowtorzen); np Tablica=read("nazwa")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Procedura Testowa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%% @about Procedura testowa dla Pana Doktora
%% @author sevikon
%% @param Name - nazwa pliku z tablica, Steps - liczba krokow
%% @return czas w mikrosekundach i {Size,Tab} - krotka z rozmiarem i nowa tablica
next(Name,Steps)->
    Tab= read(Name),
    timer:tc(life, comm, [Tab,Steps,10]). 

%% @about Kolejna procedura testowa dla Pana Doktora
%% @author sevikon
%% @param TablicaReaded - tablica wczytana z pliku np TablicaReaded=read("nazwaPliku")
%% @return czas w mikrosekundach i {Size,Tab} - krotka z rozmiarem i nowa tablica
nextTab(TablicaReaded,Steps)->
    timer:tc(life, comm, [TablicaReaded,Steps,10]). 

%% @about Kolejna procedura testowa dla Pana Doktora
%% @author sevikon
%% @param Name - nazwa pliku z tablica, Steps - liczba krokow, Times - ile razy ma sie wykonac 
%% @return czas w mikrosekundach i {Size,Tab} - krotka z rozmiarem i nowa tablica
test(Name,Steps,Times)->
    Tab= read(Name),
    test_avg(life, nextTab, [Tab,Steps], Times).

%% @about Kolejna procedura testowa dla Pana Doktora
%% @author sevikon
%% @param TablicaReaded - tablica wczytana z pliku np TablicaReaded=read("nazwaPliku"),Times - ile razy ma sie wykonac 
%% @return czas w mikrosekundach i {Size,Tab} - krotka z rozmiarem i nowa tablica
testTab(TablicaReaded,Steps,Times)->
    test_avg(life, nextTab, [TablicaReaded,Steps], Times).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mechanizm gry Life
%% @author Mateusz Ścirka
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
	
%Highly advanced function - tworzy odwrocona liste list z listy
% @params, Lista, Szerokosc docelowej
toReversedListList(List, Width) ->
	toReversedListList(List, Width, 0, [], []).
	
toReversedListList([], _Width, _Iteration, NewRow, NewList) ->
	[NewRow]++NewList;	%%zamieniajac kolejnsoc mozna odwrocic liste
toReversedListList(List, Width, Iteration, NewRow, NewList) when Width == Iteration ->
	toReversedListList(List, Width, 0, [], [NewRow]++NewList ); %i tu	
toReversedListList(List, Width, Iteration, NewRow, NewList) ->	
	[F | Rest] = List,
	toReversedListList(Rest, Width, Iteration + 1, [F]++NewRow, NewList). %i tu
	
%Dodaje ogrodzenie z zer.
% @params Lista, jej szerokosc, miejsce dodania zer
addZeros(List, Pos) ->
	addZeros(List, length(List), Pos).
addZeros(List, Size, Pos) ->
        case Pos of
                {top} -> [First | _] = List, [lists:duplicate(length(First), 0)] ++ List;
                {bottom} -> [First | _] = List, List ++ [lists:duplicate(length(First), 0)];
                {left} -> addZeros(List, Size, 0, [], {left});
                {right} -> addZeros(List, Size, 0, [], {right});
                {topleft} -> addZeros(addZeros(List, Size, {left}), Size - 1, {top});
                {topright} -> addZeros(addZeros(List, Size, {right}), Size - 1, {top});
                {bottomleft} -> addZeros(addZeros(List, Size, {left}), Size - 1, {bottom});
                {bottomright}-> addZeros(addZeros(List, Size, {right}), Size - 1, {bottom});
                {around} -> addZeros(addZeros(addZeros(addZeros(List, Size, {top}), Size, {bottom}), Size + 2, {left}), Size + 2, {right});
                {left_top_right} -> addZeros(addZeros(addZeros(List, Size, {top}), Size + 1, {left}), Size + 1, {right});
                {left_bottom_right} -> addZeros(addZeros(addZeros(List, Size, {bottom}), Size + 1, {left}), Size + 1, {right});
                {left_right} -> addZeros(addZeros(List, Size, {left}), Size, {right})
        end.
        
% @params Lista, Wielkosc (bez zer), Iteracja poczatkowa = 0, Lista z dodanymi zerami,
addZeros(_List, Size, Iter, NewListWithZeros, _Pos) when Iter == Size ->
        NewListWithZeros;
        
addZeros(List, Size, Iter, NewListWithZeros, {left}) ->
        addZeros(List, Size, Iter + 1, NewListWithZeros ++ [lists:flatten([0] ++ lists:sublist(List, Iter + 1, 1))], {left});
        
addZeros(List, Size, Iter, NewListWithZeros, {right}) ->
        addZeros(List, Size, Iter + 1, NewListWithZeros ++ [lists:flatten(lists:sublist(List, Iter + 1, 1) ++ [0])], {right}).

		
%Ciecie listy w zależności od ilości dostępnych nodów	
% @param Lista, Ilosc aktywnych nodow
divideList(List, ActiveNodes) ->
	[_F | _] = List,
	Height = length(List),
	RowsPerList = trunc(Height/ActiveNodes),
	if 
		ActiveNodes > Height -> List; %Wszystkie nody mozna wykorzystac gdy wysokośc tablicy jest o conajmniej 2 większa od ich liczby :)
		true -> divideList(List, RowsPerList, [], [])
	end.	
	
	
divideList([], _Iteration, ListListElement, CutList) ->
	AlmoustFinished = CutList++[[{left_bottom_right}, ListListElement]],
	AlmoustFinished;
	
divideList(List, 0, ListListElement, []) -> 
	[F | _Rest] = List,
	divideList(List, length(ListListElement), [], [[{left_top_right}, ListListElement++[F]]]);	
	
divideList(List, 0, ListListElement, CutList) -> 
	[F | _Rest] = List,
	divideList(List, length(ListListElement), [], CutList++[[{left_right}, ListListElement++[F]]]); 
	
divideList(List, Iteration, ListListElement, CutList) ->
	[F | Rest] = List,
    divideList(Rest, Iteration - 1, ListListElement++[F], CutList).	


	
%Zasady gry
% @params wartość komórki środkowej, suma sąsiadów
gameRules(Cell, Neighbors) -> 
        if
                Cell == 0 -> if Neighbors == 3 -> 1; true -> 0 end;
                Cell == 1 -> if Neighbors == 3; Neighbors == 2 -> 1; true -> 0 end
        end.

%Oblicza nowy stan (achtung highly advanced function)
% @params Lista
calculate(List) ->
	[F | _] = List,
	Height = length(List),
	Width = length(F),
	put(calculated, []),
	[F]++addZeros(toReversedListList(calculate(List, Height, Width), Width - 2),{left_right})++[lists:last(List)].

	
calculate(_List, 2, _Width) -> get(calculated);
calculate(List, Height, Width) -> 
	[F,S,T | Rest] = List,
	calculateRow(F, S, T, Width - 2, 0),
	calculate([S]++[T]++Rest, Height - 1, Width).

calculateRow(F, S, T, End, Iteration) ->
	if 
		Iteration < End -> 
			[F1, F2, F3 | RestF] = F,
			[S1, S2, S3 | RestS] = S,
			[T1, T2, T3 | RestT] = T,
			put(calculated, [gameRules(S2,F1+F2+F3+S1+S3+T1+T2+T3)]++get(calculated)),
			calculateRow([F2]++[F3]++RestF, [S2]++[S3]++RestS, [T2]++[T3]++RestT, End, Iteration + 1);
		true -> ok
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%% Koniec MŚ %%%%%%%%%
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
        % 49 -> intoArrayOfChar(Tail, Lista ++ [1]);
        % 48 -> intoArrayOfChar(Tail, Lista ++ [0]);
        50 -> intoArrayOfChar(Tail, Lista ++ [1]);
        49 -> intoArrayOfChar(Tail, Lista ++ [0]);
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
saveArray(FileName,Array,Size) ->
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
        % 1 -> intoString(Tail, Lista ++ "1");
        % 0 -> intoString(Tail, Lista ++ "0");
        1 -> intoString(Tail, Lista ++ "2");
        0 -> intoString(Tail, Lista ++ "1");
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

	
%%%%% 				ZMIENIC


% @about Dzielenie tablicy na mniejsze
% @autor malgmaci        
% @params Lista, Liczba aktywnych nodów, Size - Szerokość głównej tablicy (Size x Size)
%divideArray(List, ActiveNodes, Size) ->
 %       MinCols = trunc(math:sqrt(length(List)) / ActiveNodes),
 %       %Length = trunc(length(List) / ActiveNodes),
 %       NumOfRows = Size div ActiveNodes,
 %       Rest = Size - (ActiveNodes * NumOfRows),
 %       Length = NumOfRows*Size,
 %       if
 %               MinCols >= 2 -> divide(List, [ [{left_top_right}] ++ [lists:sublist(List, 1, Size*(NumOfRows+1))]] , ActiveNodes, 1, Length, Size, NumOfRows, Rest);
 %               true -> List
  %      end.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dzielenie tablicy
%% @author Malgorzata Maciurzynska
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%@params Lista, Nowa lista, Liczba aktywnych nodów, Obecna iteracja, Ilosc znakow pobieranych , Wielkość głównej tablicy, Ilość wersów dla małej tablicy, Wersy, które pozostały zostaną przyłączone do ostatniej tablicy
%divide(_List, NewList, ActiveNodes, CurrentPart, _Length, _Size, _NumOfRows, _Rest) when ActiveNodes == CurrentPart ->
%    NewList;

%divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) when ActiveNodes == CurrentPart + 1 ->
%    divide(List, NewList ++ [ [{left_bottom_right}] ++ [lists:sublist(List, (CurrentPart * Length+1)-Size, (NumOfRows*Size)+Size+Rest*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest);
    
%divide(List, NewList, ActiveNodes, CurrentPart, Length, Size, NumOfRows, Rest) ->
%    divide(List, NewList ++ [ [{left_right}] ++ [lists:sublist(List, (CurrentPart * Length+1)-Size, (NumOfRows*Size)+2*Size)]], ActiveNodes, CurrentPart + 1, Length, Size, NumOfRows, Rest).

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
                Nowa = replaceLast(Tablica,Row);
            Index==2 ->
                [RowF,RowL]= Row,
                Nowa2 = replaceFirst(Tablica,RowF),
                Nowa = replaceLast(Nowa2,RowL);
            true ->
                Nowa = replaceFirst(Tablica,Row)
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
        WithZeros= addZeros(Tablica, Param),
        Policzona = calculate(WithZeros), % Liczenie
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
            Divided = divideList(CalaTablica, ActiveNodes),
            nodesCalc(Divided,ListaPidow),
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
                {_,NowyIterator} = get(iterator),
                {_,Wyniki} = get(wyniki),
                put(wyniki, {ok, replaceRow(Wyniki,WynikowaTablica,Fragment)}),
                NowyIterator2= NowyIterator -1,
                put(iterator, {ok, NowyIterator2}),
                if
                    NowyIterator2 == 0 ->
                        put(status, {ok, stop}),
                        loop2(ListaPidow,ActiveNodes,Size,Steps);
                    true ->
                        loop2(ListaPidow,ActiveNodes,Size,Steps)
                end;
            {From, {policzono, Pid, Tablica}} ->
                From ! {self(), ok},
                {_,Fragment} = get(Pid),
                [TopRow,DownRow] = Tablica,
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
                        if
                            Steps == 1 ->
                                put(status, {ok, wait}),
                                put(iterator, {ok, length(ListaPidow)}),
                                getTablesBack(ListaPidow), % prosba o zwrocenie wynikow
                                loop2(ListaPidow,ActiveNodes,Size,Steps-1);
                            true ->
                                put(status, {ok, again}),
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

%% @about Odpalenie procesow, operacje na nich i czekanie na wyniki
%% @author sevikon
%% @param TablicaReaded - tablica wczytana z pliku, Steps-liczba krokow,  Cheats - liczba Pidow na node'ie
%% @return {Size,Wynik} - Size -> rozmiar, Wynik - policzona tablica
comm(TablicaReaded,Steps,Cheats) ->
    {Size, List} = TablicaReaded, %tymczasowo wczytuje z pliku,
    ActiveNodes = length(checkGoodNodes(nodes()))*Cheats,
    ListaPidow = createPids(Cheats), %utworzenie pidow
    put(iterator, {ok, length(ListaPidow)}), %na ile procesow trzeba czekac
    put(status, {ok, start}), %obecny status iteracji
    put(scalonatablica, {ok, List}), %cala tablica
    put(wiersze, {ok, emptyArray(2*length(ListaPidow))}), %cala tablica
    put(wyniki, {ok, emptyArray(length(ListaPidow))}),
    Wynik = loop2(ListaPidow,ActiveNodes,Size,Steps), %czekanie na wyniki
    {Size,Wynik}.

%% @about Utworzenie nowych Pidow na nodeach
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
createPids(Times) ->
    Nodes = checkGoodNodes(nodes()),
    Cheated = cheatNodes(Nodes,Times),
    createPids(Cheated,[]).
createPids([], ListaPidow) -> ListaPidow;
createPids([Head|Tail], ListaPidow) ->
    Pid = spawn(Head,life,loop,[]), %utworzenie na nodach procesu, ktory czeka
    put(Pid, {ok, length(ListaPidow)+1}), %utworzenie mapy zeby miec dostep pozniej
    createPids(Tail, ListaPidow ++ [Pid]).

%% @about Wysyłanie do wszystkich procesów zadania obliczen wraz z kawalkami tablic (1 iteracja)
%% @author sevikon
%% @param ListaPidow - Lista pidow na node'ach
%% @return ok if correct
nodesCalc(Divided,ListaPidow) ->
    nodesCalc(Divided,ListaPidow, length(ListaPidow)).
nodesCalc([],_ListaPidow,_Other) -> ok;
nodesCalc(_Divided,_ListaPidow,0) -> ok;
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
getTablesBack(_ListaPidow,0) -> ok;
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
nodeNewsRows([],_ListaPidow,_Other) -> ok;
nodeNewsRows(_Wiersze,_ListaPidow,0) -> ok;
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
initNodes(_ListaPidow,0,_Size) -> ok;
initNodes(ListaPidow,Index,Size) ->
    Pid = lists:nth(Index, ListaPidow),
    rpc(Pid,{init,self(),Size}),
    initNodes(ListaPidow,Index-1,Size).


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
%% @return Array - pusta lista jednowymiarowa
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
cheatNodes(_Array,0,NewArray)->NewArray;
cheatNodes(Array,Times,NewArray)->
    cheatNodes(Array,Times-1,NewArray++Array).


%% @about Wybieranie tylko node'ow pasujacych do wzorca l@le
%% @author sevikon
%% @param Lista - lista node'ow (np dzieki nodes() uzyskana)
%% @return lista wlasciwych node'ow
checkGoodNodes(Lista)->
checkGoodNodes(Lista, []).
checkGoodNodes([], Zwracana)->Zwracana;
checkGoodNodes(Lista,Zwracana)->
[Head|Tail]= Lista,
[HeadString]= io_lib:format("~p",[Head]),
Num = string:str(HeadString, "l@le"),
if 
    Num>0 ->
        checkGoodNodes(Tail,Zwracana++[Head]);
    true->
        checkGoodNodes(Tail,Zwracana)
end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Do testowania
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
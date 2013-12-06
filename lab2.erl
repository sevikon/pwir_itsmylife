-module(lab2).
-export([area/1, areaAndVal/1, len/1, max/1, min/1, min_max/1,min_max2/1,area2/1,listaN/1, temp_conv/1]).

%zadanie 1
area({rect,X,Y}) ->
    X*Y;
area({cir,X}) -> 
	3.14*X*X;
area({tri,A,B,C}) -> 
	P = 0.5*(A+B+C),
	math:sqrt(P*(P-A)*(P-B)*(P-C));
area({tri,A,H}) -> 
	0.5*A*H;
area({tri,A,B,Deg,deg}) -> 
	0.5*A*B*math:sin(Deg/360*3.14);
area({tri,A,B,Deg,rad}) -> 
	0.5*A*B*math:sin(Deg);
area({tra,A,B,H}) -> 
	0.5*(A+B)*H;
area({ball,R}) ->
	4*3.14*R*R;
area({cone,R,H}) ->
	3.14*R*R+3.14*R*math:sqrt(R*R+H*H);
area({cube,A}) ->
	A*A*A.

areaAndVal({ball,R}) ->
	io:format("pole: ~w, obj: ~w ", [prec(4*3.14*R*R), prec(4/3*3.14*R*R*R)]);
areaAndVal({cone,R,H}) ->
	PP =3.14*R*R,
	io:format("pole: ~w, obj: ~w~n ", [prec(PP+3.14*R*math:sqrt(R*R+H*H)),prec(1/3*PP*H)]);
areaAndVal({cube,A}) ->
	io:format("pole: ~w, obj: ~w~n ", [prec(6*A*A)],prec(A*A*A)).

prec(Value) ->
	round(Value*1000)/1000.

%zadanie 2
%Napisz program liczący długość listy (len/1).
len([]) ->
0;
len([Head | Rest]) ->
1 + len(Rest).

%zadanie 3
%Napisz program podający największy element listy (max/1).
max([Head|Rest]) ->
	max(Rest, Head).

max([], TheBiggest) ->
	TheBiggest;
max([Head|Rest], TheBiggest) when Head > TheBiggest ->
	max(Rest, Head);
max([Head|Rest], TheBiggest) ->
	max(Rest, TheBiggest).

%zadanie 4
%Napisz program podający najmniejszy element listy (min/1).
min([Head|Rest]) ->
	min(Rest, Head).
	
min([], TheSmallest) ->
	TheSmallest;
min([Head|Rest], TheSmallest) when Head < TheSmallest ->
	min(Rest, Head);
min([Head|Rest], TheSmallest) ->
	min(Rest, TheSmallest).

%zadanie 5
%Napisz program zwracający krotkę 2-elementową z najmniejszym i największym elementem listy (min_max/1).
min_max([Head|Rest])->
	{min([Head|Rest]),max([Head|Rest])}.

%zadanie 6
%Napisz program zwracający krotkę 2-elementową z najmniejszym i największym elementem listy (min_max/1).
min_max2([Head|Rest])->
	[min([Head|Rest]),max([Head|Rest])].

%zadanie 7
%Napisz program wyliczający pola figur/brył podanych jako lista krotek. Zwracana ma być lista pól.
area2([Elem])->
	[prec(area(Elem))];
area2( [Area | Rest] ) ->
	[ prec(area(Area)) | area2(Rest) ].


%zadanie 8
%Napisz program, który dla danego N zwróci listę formatu [N,N-1,…,2,1].
listaN( 0 ) -> 
	[];
listaN( N ) when N > 0 -> 
	[ N | listaN( N-1 ) ].

%Zadanie 9
%Napisz konwerter temperatury pomiędzy różnymi skalami. 
%Temperatura podawana jest jako krotka {typ, wartość} np. {c, 22.4}, {f,0.0}. 
%Funkcja konwertująca ma przyjmować 2 parametry: krotkę reprezentującą 
%temperaturę oraz skalę docelową np. temp_conv({c,22.4},k). 
%Wartością zwracaną ma być odpowiednia krotka np. {k,233.47}.
temp_conv({{Actual, Temp},Name}) ->
	temp_conv_to_name(temp_conv_to_c({{Actual, Temp},Name})).

% na poczatku wszystko do Celcjusza
temp_conv_to_c({{c, Temp},Name}) ->
	{Name, Temp};
temp_conv_to_c({{f, Temp},Name}) ->
	{Name, 5 / 9 *(Temp - 32)};
temp_conv_to_c({{k, Temp},Name}) ->
	{Name, Temp - 273.15}.

% potem z Celcjusza na pozostale
temp_conv_to_name({c, Temp}) ->
	{c, Temp};
temp_conv_to_name({f, Temp}) ->
	{f, 32 + 9/ 5 *(Temp)};
temp_conv_to_name({k, Temp}) ->
	{k, Temp + 273.15}.
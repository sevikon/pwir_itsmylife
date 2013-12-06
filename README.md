pwir_itsmylife
==============

Projekt z PWIR / Gra life w Erlangu / IS 2013

realizowany na potrzeby zaliczenia przediotu POW

Organizacja tego laboratorium

programy powstają w zespołach 4-5 osobowych
kod programu ma powstawać w repozytorium git umieszczonym na serwerze borg.kis.agh.edu.pl
repozytorium ma być dostępne dla mnie (man chacl) - mój login to ptm
jedna z osób z zespołu przesyła mi skład grupy i url repozytorium
opisy commit mają być znaczące (nie ” poprawka”, tylko „poprawienie algorytmu ….”)
razem z programem ma powstać raport z prac
ma zawierać listę osób
opis przyjętego rozwiązania
opis testów użytych komponentów (jeśli takie były)
opis testu skalowalności (czy użycie kolejnych węzłów poprawia wynik)
opis ma być umieszczony w repozytorium
wszystkie grupy muszą oddać działający program aby zaliczyć to laboratorium
najlepszy rezultat będzie premiowany +1 na kolokwium zaliczeniowym
drugi rezultat będzie premiowany +1/2 na kolokwium zaliczeniowym
Cel

Opracowanie architektury rozproszonego,skalowalnego systemu w erlangu dla gry Life w/g podstawowej reguły Conwaya 23/3.

Wymagania

rozmiar planszy jest kwadratowy będący potęgą 2 począwszy od 256×256 do 16384×16384 (rozmiar 8-14)
program musi wykorzystywać rozproszenie
nie trzeba wykorzystywać wszystkich węzłów
program powinien uwzględniać, że nie wszystkie węzły będą zawsze dostępne
zakładamy, że węzły nie będą znikać w trakcie obliczeń
program ma posiadać możliwość generowania losowych plansz
program ma posiadać wbudowany benchmark
funkcję test_time/1 wykonująca podaną ilość iteracji funkcji next/0
wykorzystać należy gotową funkcję ze strony https://erlangcentral.org/wiki/index.php/Measuring_Function_Execution_Time
program ma posiadać funkcję „next/0” , która wylicza następną iterację
program musi mieć możliwość wczytania planszy z pliku i zapisu do pliku
plik jest skompresowanym ciągiem zawierającym rozmiar jako pierwszy bajt (2^X, np. 12 oznacza planszę 2^12 na 2^12) oraz wartości poszczególnych komórek (0 lub 1) wierszami
kompresja - patrz file:open/2

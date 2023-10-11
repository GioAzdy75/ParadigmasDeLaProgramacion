
%1- Definir la relación primero(L,X) que verifique si X es el primer elemento de la lista L.
% Obtener la respuesta a las siguientes preguntas:
primero([Cabeza|_],Cabeza).

%2- Definir la relación resto(L1,L2) que verifique si L2 es la lista obtenida a partir de la lista
% L1 suprimiendo el primer elemento
resto([_|Resto],Resto).

%3- Definir la relación construye(X,L1,L2) que verifique si L2 es la lista obtenida añadiéndole
% X a L1 como primer elemento.
construye(Cabeza,L1,[Cabeza|L1]).

%4- Definir la relación pertenece(X,L) que verifique si X es un elemento de la lista L.
%Utilizar el programa para responder a las siguientes cuestiones:
pertenece(X,[X]).
pertenece(X,[_|Resto]) :- pertenece(X,Resto).

%5- Definir la relación concatena(L1,L2,L3) (equivalente a append) que verifique si L3 es la
% lista obtenida escribiendo los elementos de L2 a continuación de los elementos de L1.
concatena([],L2,L2).
concatena([Cabeza|Resto],L2,K_1) :- concatena(Resto,L2,K) , K_1 = [Cabeza|K].

%6- Un palíndromo es una palabra que se lee igual en los dos sentidos, por ejemplo “oso”. Definir
% la relación palíndromo(L) que verifique si la lista L es un palíndromo.
invertir_lista([],[]).
invertir_lista([Cabeza|Resto],K_1) :- invertir_lista(Resto,K), append(K,[Cabeza],K_1).
palindromo(L1) :- invertir_lista(L1,L2) , L1 = L2. 

%7- Definir la relación último(X,L) (equivalente a last) que verifique si X es el último elemento
% de la lista L.
ultimo(X,[X]).
ultimo(X,[_|Resto]) :- ultimo(X,Resto),!.

%8- Utilizando el predicado select, definir la relación inserta(X,L1,L2) que verifique si L2 es una
% lista obtenida insertando X en L1. Compruebe el resultado de la consulta inserta(a,[1,2],L).

inserta(X, L1, L2) :- select(X, L2, L1).

%9- Utilizando el predicado append, definir la relación sublista(L1,L2) que verifique si L1 es
% una sublista de L2.

sublista(L1,L2) :- append(_,Sublista,L2) , append(L1,_,Sublista).

%10- Definir la relación subconjunto(L1,L2) que verifique si L2 es un subconjunto de L1.
subconjunto([],_).
subconjunto([X|L1],L2) :- member(X,L2),subconjunto(L1,L2).

%11- Definir la relación maximo(X,Y,Z) (equivalente a max) que verifique si Z es el maximo de
% X e Y
maximo(X,Y,Z) :- X > Y , Z = X ,!.
maximo(X,Y,Z) :- Y > X , Z = Y.

%12-  Definir la relación mcd(X,Y,Z) que verifique si Z es el maximo común divisor de X e Y.
mcd(X,0,X) :- X > 0.
mcd(X,Y,Z) :- Y > 0, Resto is X mod Y , mcd(Y,Resto,Z).

%13- Definir la relación longitud(L,N) que se verifique si N es la longitud de la lista L.
longitud([],0).
longitud([_|Resto],N1)  :-  longitud(Resto,N), N1 is N + 1.

%14- Una lista está acotada si todos sus elementos son menores que su longitud. Definir la
% relación lista_acotada(L) que verifique si todos los elementos de la lista de números L son
% menores que la longitud de L. (usar length/2)

recorrer_lista([],_).
recorrer_lista([Cabeza|Resto],N) :- recorrer_lista(Resto,N) , Cabeza =< N.
lista_acotada(L) :- length(L,Length) , recorrer_lista(L,Length).

%15- Definir la relación max_lista(L,X) que se verifique si X es el maximo de la lista de números L.
recorrer_lista_max([],_).
recorrer_lista_max([Cabeza|Resto],N) :- recorrer_lista_max(Resto,N) , maximo(Cabeza,N,N).
max_lista(L,N) :- recorrer_lista_max(L,N).

%16. Definir la relación suma_lista(L,X) (equivalente a sumlist) que se verifique si X es la suma
% de los elementos de la lista de números L.

suma_lista([Cabeza],Cabeza).
suma_lista([Cabeza|Resto],X_1) :- suma_lista(Resto,X) , X_1 is Cabeza + X , !.


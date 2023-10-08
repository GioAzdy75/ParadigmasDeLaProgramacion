
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

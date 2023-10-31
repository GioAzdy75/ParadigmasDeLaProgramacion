/*
1. Define un predicado multirot(Xs,MRXs) que, dadas las listas Xs y MRXs, se satisface
cuando MRXs es la concatenación de varias rotaciones (al menos una) de Xs. Por ejemplo:
?- multirot([a,b,c],[a,b,c,c,a,b,b,c,a,c,a,b,a,b,c]).
Yes
?- multirot([a,b,c],[a,b,c,a,c]).
No
*/

% Caso base: Cuando Xs es una rotación de MRXs.
multirot(Xs, MRXs) :-
    is_rotation(Xs, MRXs).

% Regla recursiva: Verifica si MRXs es una concatenación de rotaciones de Xs.
multirot(Xs, MRXs) :-
    append(Rotation, RestMRXs, MRXs),
    is_rotation(Xs, Rotation),
    multirot(Xs, RestMRXs).

% Predicado para verificar si una lista Xs es una rotación de Ys.
is_rotation(Xs, Ys) :-
    append(Start, End, Ys),
    append(End, Start, Xs).

/*
2. Define un predicado son consecutivas(N,Xs) que se satisfaga cuando Xs sea una lista donde
cada número i entre 1 y N aparece i veces consecutivas. Por ejemplo:
?- son_consecutivas(5,[3,3,3,1,2,2,5,5,5,5,5,4,4,4,4]).
Yes
*/
son_consecutivas(0,[]).
son_consecutivas(N,Xs) :- N > 0 , append(ListaI,Resto,Xs), length(ListaI,N), maplist(=(N),ListaI), M is N-1, son_consecutivas(M,Resto).
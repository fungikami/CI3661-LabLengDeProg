% arco(A, B) significa que hay un arco desde el nodo A hasta el nodo B
% arco(A, B).

% hermano(A, B) si existe nodo C tal que hay un arco desde C hasta A y un arco desde 
% C hasta B. A y B son distintos
hermano(A, B) :- arco(C, A), arco(C, B), A \= B.

% alcanzable(A, B) existe algun camino, siguiendo 0 o más arcos desde A hasta B.
% Suponemos que no hay ciclos
alcanzable(A, B) :- arco(A, B).
alcanzable(A, B) :- arco(A, C), alcanzable(C, B).

% lca(A, B, C) si C alcanza a A y B, no existe C' alcanzable desde C, que a su vez 
% alcance a A y B
lca(A, B, C) :- 
    alcanzable(C, A), 
    alcanzable(C, B), 
    not((alcanzable(C, C1), alcanzable(C1, A), alcanzable(C1, B))).

% tree(A) que se satisfaga si los nodos alcanzables desde A (en conjunto con los arcos
% en cuestión) forman un árbol.
% No ciclos, no otro vertice que llegue a la raiz

% ciclo(A, visitados) si existe un ciclo que pasa por A y no pasa por ninguno de los
% nodos en visitados
% ciclo(A, visitados) :- arco(A, B), member(B, visitados).
% ciclo(A, visitados) :- 
%     arco(A, B), 
%     not(member(B, visitados)), 
%     ciclo(B, [B|visitados]).

arborescencia(A) :- 
    findall(B, arco(A, B), Hijos),
    forall(member(H, Hijos), arborescencia(H)).

% arborescencia(A) :-
%     findall(B, arco(_, B), Nodos),
%     forall(member(Nodo, Nodos), alcanzable(A, Nodo)),
%     findall(B, arco(A, B), Hijos),
%     forall(member(Hijo, Hijos), arborescencia(Hijo)).

tree(A) :- 
    arco(A, B),             % Hijos de A
    not(alcanzable(B, A)),  % No camino de B a A
    not(ciclo(A)),          % No ciclo que pase por A
    arborescencia(A).       % Arborescencia desde A

% Revisa si hay un ciclo que pase por A
ciclo(A) :- alcanzable(A, B), alcanzable(B, A).

% % Revisa si A y B están conectados
% conectado(A) :- not(alcanzable(A, B)), not(alcanzable(B, A)).

% % Revisa si A es la raiz de un arbol
% tree(A) :- not(ciclo(A)), conectado(A).

% Example 
arco(a, b).
arco(b, c).
arco(b, d).
example :- 
    tree(a).

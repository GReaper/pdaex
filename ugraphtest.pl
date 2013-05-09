% Crear un grafo basico inicial. Contiene 5 vértices y 3 aristas.
% Se conectan los vértices A con C, A con B, B con D y D con B.
test1(G) :- vertices_edges_to_ugraph([e],[a-b,a-c,b-d,d-b],G),
	write('grafo -> '),writeln(G).

% Funcion para listar los vertices y aristas de un grafo. Usada para
% testeo
listaElems(G) :-
	vertices(G,V),
	edges(G,A),
	write('vertices -> '),writeln(V),
	write('aristas -> '),writeln(A).


% Listar los vértices y aristas del grafo creado anteriormente.
test2 :- test1(G),
	listaElems(G).

% Agregar un nuevo vértice sin conexiones (F)
test3 :- test1(G),
	add_vertices(G,[f],NG),
	listaElems(NG).

% Conectar el vertice F con B y E con F
test4 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NGF),
	listaElems(NGF).

% Transponer el grafo anterior para invertir las conexiones
test5 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NG2),
	transpose(NG2,NGF),
	listaElems(NGF).

% Obtener vecinos de los vértices A y B
test6 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NGF),
	neighbours(a,NGF,VA),
	write('vecinos de a -> '),writeln(VA),
	neighbours(b,NGF,VB),
	write('vecinos de b -> '),writeln(VB).

% Eliminar la arista entre B y D
test7 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NG2),
	del_edges(NG2,[b-d],NGF),
	listaElems(NGF).

% Eliminar el vértice B
test8 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NG2),
	del_vertices(NG2,[b],NGF),
	listaElems(NGF).

% Comprobar que C es alcanzable desde A en el ejemplo 8
test9 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NG2),
	del_vertices(NG2,[b],NGF),
	reachable(a,NGF,AD),
	member(c,AD).

% Lista de vértices alcanzables desde F en el ejemplo 4
test10 :- test1(G),
	add_vertices(G,[f],NG1),
	add_edges(NG1,[e-f,f-b],NGF),
	reachable(f,NGF,R),
	write('alcanzables desde f -> '),writeln(R).

% Comprobar resultado al tratar de eliminar un vértice no existente
% NOTA: no devuelve false, sino que tan solo elimina los vértices
% en caso de existir (comprobado).
test11 :- test1(G),
	del_vertices(G,[t],NG),
	listaElems(NG).

% Comprobar resultado al tratar de eliminar una arista no existente
% NOTA: no devuelve false, sino que tan solo elimina las aristas
% en caso de existir (comprobado).
test12 :- test1(G),
	del_edges(G,[c-e],NG),
	listaElems(NG).

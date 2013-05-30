%DCG
data([A|As]) -->
    spaces(_),
    chars([X|Xs]),
    {atom_codes(A, [X|Xs])},
    spaces(_),
    data(As).
data([]) --> [].

chars([X|Xs]) --> char(X), !, chars(Xs).
chars([]) --> [].

spaces([X|Xs]) --> space(X), !, spaces(Xs).
spaces([]) --> [].

space(X) --> [X], {code_type(X, space)}.
char(X) --> [X], {\+ code_type(X, space)}.

%Predicates
main:-
    prompt(_, 'web> '),
    repeat,
    read_line_to_codes(current_input, Codes),
    (
        Codes = end_of_file
    ->
        true
    ;
        processData(Codes),
        fail
    ).

processData(Codes) :-
    % convert the list of codes to a list of code lists of words
    (phrase(data(AtomList), Codes) ->
        % concatenate the atoms into a single one delimited by commas
        concat_atom(AtomList, ', ', Atoms),
        %write_ln(Atoms)
        process_atomList(AtomList)
    ;
        format('Didn''t recognize data.\n')
    ).

%process_atomList([scan|Options]):-
%       !,
%	runScan(Options).

%process_atomList([find|Options]):-
%       !,
%	runFind(Options).

process_atomList([help|_]):-!,
        writeln('LLamada a la funcion para mostrar la ayuda').

process_atomList([_|_]):-
	writeln('Comando no existente.Consulte la ayuda').



%---------------%
%  MAIN PROMPT  %
%---------------%

% Main application entry point
crawler :-
    prompt(_, 'crawler1.0 > '),
    repeat,
    read_line_to_codes(current_input, Codes),
    (
        Codes = end_of_file
    ->
        % End prompt with EOF
        true
    ;
        % Process command
        process_command(Codes),
        % Fail to relaunch crawler prompt
        fail
    ).

process_command(Codes) :-
    % Convert the list of codes to a list of code lists of words
    (
        phrase(split_by_spaces(AtomList), Codes) ->
        % Check if the command is OK and launch the appropiate predicate
        check_and_execute(AtomList)
        ;
        writeln('Error: invalid command. Please, type "help" to get the command using list.')
    ).

% DCG to split the user entry by spaces to retrieve all
% commands and parameters
split_by_spaces([A|As]) -->
    rm_spaces(_),
    get_chars([X|Xs]),
    {
        atom_codes(A, [X|Xs])
    },
    rm_spaces(_),
    split_by_spaces(As).
split_by_spaces([]) --> [].

get_chars([X|Xs]) --> 
    get_char(X), !, 
    get_chars(Xs).
get_chars([]) --> [].

rm_spaces([X|Xs]) --> 
    get_space(X), !, 
    rm_spaces(Xs).
rm_spaces([]) --> [].

get_space(X) --> 
    [X], 
    {
        % Check for space code
        code_type(X, space)
    }.
get_char(X) --> 
    [X], 
    {
        % Check for any code except space one
        \+ code_type(X, space)
    }.

% Check and execute a command
check_and_execute([scan | Options]) :-
    format_options(Options, FOptions),
    writeln(FOptions),
    !,
    get_needed_param('-u', FOptions, URL),
    writeln(URL). 
check_and_execute([find | Options]) :-
    format_options(Options, FOptions),
    writeln(FOptions),
    !. 
check_and_execute([help | _]) :-
    writeln('=== Crawler help ==='),
    writeln('1.- Command to retrieve data from a starting URL'),nl,
    writeln('scan -u URL [-d Depth]'),nl,
    writeln('  1.1.- Parameters:'),
    writeln('    -u : URL used as starting point. It must be a valid web URL and'),
    writeln('         '),
    writeln(''),
    writeln(''),
    writeln(''),
    writeln(''),
    !. 
check_and_execute(_) :-
    writeln('Command not found or invalid params. Please, type "help" to check for available commands and formats.'),
    !. 

% Predicate to format the received options list
format_options([],[]).
format_options([ Type, Value | ROp], FOp) :-
    format_options(ROp, FO1),
    append([(Type,Value)], FO1, FOp),
    !.
format_options :-
    writeln('Invalid options format.  Please check your syntax or type "help" to list all available commands.'),
    fail.

% Predicate to get one needed param from the list
get_needed_param(Type , [], _) :-
    !,
    name(Type, C1),
    append("Invalid ", C1, A1),
    append(A1, " parameter value or param not found. Please check your Please check your syntax or type \"help\" to list all available commands.", Error),
    name(EText, Error),
    writeln(EText),
    fail.
get_needed_param(Type, [ (Type, Value) | _ ], Value) :- 
    (
        atomic(Value) ->
        !
        ;
        !,
        % Write error
        name(Type, C1),
        append("Invalid ", C1, A1),
        append(A1, " parameter value. Please, check it is a valid atom.", Error),
        name(EText, Error),
        writeln(EText),
        fail
    )
    .
get_needed_param(Type, [ _ | ParamList ], Value) :-
    !,
    get_needed_param(Type, ParamList, Value).

% Predicate to get one optional param from the list
get_param(_ , [], _) :-
    !,
    fail.
get_param(Type, [ (Type, Value) | _ ], Value) :- 
    (
        atomic(Value) ->
        !
        ;
        !,
        % Write error
        name(Type, C1),
        append("Invalid ", C1, A1),
        append(A1, " parameter value. Please, check it is a valid atom.", Error),
        name(EText, Error),
        writeln(EText),
        fail
    )
    .
get_param(Type, [ _ | ParamList ], Value) :-
    !,
    get_param(Type, ParamList, Value).

% Predicate to get one number param from the list. In this case 
% we must ensure the numbes is greater or equal than zero
get_number_param(_ , [], _) :-
    !,
    fail.
get_number_param(Type, [ (Type, Value) | _ ], Value) :- 
    (
        (integer(Value), Value >= 0) ->
        !
        ;
        !,
        % Write error
        name(Type, C1),
        append("Invalid ", C1, A1),
        append(A1, " parameter value. Please, check it is a valid integer greater or equal than zero.", Error),
        name(EText, Error),
        writeln(EText),
        fail
    )
    .
get_number_param(Type, [ _ | ParamList ], Value) :-
    !,
    get_number_param(Type, ParamList, Value).

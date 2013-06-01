
%---------------%
%  MAIN PROMPT  %
%---------------%

% Main application entry point
crawler :-
    prompt(_, 'crawler1.0 > '),
    repeat,
    read_line_to_codes(current_input, Codes),
    (
        Codes = "exit"
    ->
        % End prompt with "exit" command
        writeln('=== Thanks for using Crawler1.0 ==='),
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
    % writeln(FOptions),
    !,
    % Get -u param. This parameter is required
    (get_needed_param('-u', FOptions, URL) ->
        (
        write('Starting URL: '),
        writeln(URL)
        )
        ;
        fail
    ),
    % Get -d param. This parameter is optional
    (get_number_param('-d', FOptions, Depth) ->
        (
        write('Scanning depth: '),
        writeln(Depth)
        )
        ;
        (Depth = 0,
        write('Scanning depth: '),
        writeln(Depth))
    ),
    write('Execute: scan -u '),write(URL),write(' -d '),writeln(Depth).

check_and_execute([find | Options]) :-
    format_options(Options, FOptions),
    % writeln(FOptions),
    !,
    % Get -u param. This parameter is required
    (get_needed_param('-u', FOptions, URL) ->
        (
        write('Starting URL: '),
        writeln(URL)
        )
        ;
        fail
    ),
    % Get optional parameters
    (get_number_param('-d', FOptions, Depth) ->
        (
        write('Scanning depth: '),
        writeln(Depth)
        )
        ;
        (Depth = 0,
        write('Scanning depth: '),
        writeln(Depth))
    ),
    (get_param('-s', FOptions, Starts) ->
        (
        write('Finding links starting with: '),
        writeln(Starts)
        )
        ;
        (Starts = '',
        write('Finding links starting with: '),
        writeln(Starts))
    ),
    (get_param('-c', FOptions, Contains) ->
        (
        write('Finding links containing: '),
        writeln(Contains)
        )
        ;
        (Contains = '',
        write('Finding links containing: '),
        writeln(Contains))
    ),
    (get_param('-e', FOptions, Ends) ->
        (
        write('Finding links ending with: '),
        writeln(Ends)
        )
        ;
        (Ends = '',
        write('Finding links ending with: '),
        writeln(Ends))
    ),
    write('Execute: find -u '),write(URL),
    write(' -d '),write(Depth),
    write(' -s '),write(Starts),
    write(' -c '),write(Contains),
    write(' -e '),writeln(Ends).

check_and_execute([help | _]) :-
    writeln('=== Crawler help ==='),nl,
    writeln('1.- Command to retrieve data from a starting URL'),nl,
    writeln('scan -u URL [-d Depth]'),nl,
    writeln('  1.1.- Parameters:'),
    writeln('    -u : URL used as starting point. It must be a valid web URL. HTTP over SSL is not supported in 1.0 version.'),
    writeln('    -d : (Optional) Scanning detph. It must be a value greater or equal than zero. It is zero by default, which means that only the base URL will be scanned.'),nl,
    writeln('  1.2.- Examples:'),   
    writeln('scan -u http://www.fdi.ucm.es -d 2 -> Scans for all data starting at FDI web and with depth 2'),
    nl,
    writeln('2.- Command to retrieve filtered URLs from a starting one'),nl,
    writeln('find -u URL [-d Depth] [-s Starts] [-c Contains] [-e Ends]'),nl,
    writeln('  2.1.- Parameters:'),
    writeln('    -u: URL used as starting point. It must be a valid web URL. HTTP over SSL is not supported in 1.0 version.'),
    writeln('    -d : (Optional) Scanning detph. It must be a value greater or equal than zero. It is zero by default, which means that only the base URL will be scanned.'),
    writeln('    -s : (Optional) Search for links wich starts with the given pattern.'),
    writeln('    -c : (Optional) Search for links wich contains the given pattern.'),
    writeln('    -e : (Optional) Search for links wich ends with the given pattern.'),nl,
    writeln('  2.2.- Examples:'),
    writeln('find -u http://www.fdi.ucm.es -e .pdf -> Finds all linked PDF docs in the FDI website'),
    writeln('find -u http://www.fdi.ucm.es -d 1 -c turing -> Finds all links which contains the "turing" word starting at the FDI website and with depth 2'),nl,
    writeln('3.- Command to exit crawler'),nl,
    writeln('exit'),nl,
    writeln('4.- Additional info'),nl,
    writeln('  4.1.- CPU, RAM and time usage'),
    writeln('Users must take into account that exploring in depths greater than 2 or 3 starting at well known webs may return an enormous amount of results. This may produce:'),
    writeln('  - High RAM usage: this may need user to increase the amount of RAM used by SWI-Prolog to avoid stack overflow problems.'),
    writeln('  - High CPU usage.'),
    writeln('  - Long amount of time: depending on the user\'s PC specs, exploring may take a long time in greater depths, so be patient!'),
    nl,
    writeln('  4.2.- Internet connection fails'),
    writeln('As you can imagine, you need to stay connected to the Internet while using this application. In case that your connection is lost during one scanning process, the system may continue trying to connect.'),
    writeln('If the application gets stucked on a web during a long time due to some connection problem, you can try Ctrl+C and then type "a" to abort that URL scanning. In this case the system will try to continue the execution to avoid losing data due to an unusual connection problem.'),
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
    !,
    (
        atomic(Value) ->
        true
        ;
        % Write error
        name(Type, C1),
        append("Warning: Invalid ", C1, A1),
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
get_number_param(Type, [ (Type, Value) | _ ], Num) :- 
    !,
    (
        (
        catch(
            % Try section
            (atom_number(Value, Num), integer(Num), Num >= 0),
            % Exception
            _,
             % Catch section
            ( fail )
            )
        ) ->
        true
        ;
        % Write error
        name(Type, C1),
        append("Warning: Invalid ", C1, A1),
        append(A1, " parameter value. Please, check it is a valid integer greater or equal than zero.", Error),
        name(EText, Error),
        writeln(EText),
        fail
    )
    .
get_number_param(Type, [ _ | ParamList ], Value) :-
    !,
    get_number_param(Type, ParamList, Value).

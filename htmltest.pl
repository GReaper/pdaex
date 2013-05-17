:- use_module(library(http/html_write)).

html_create_document(URI,Title,Styles,Js,Metas) :-
	phrase(html_structure(Title,Styles,Js,Metas), Tokens),
	open(URI, write, Stream),
	print_html(Stream,Tokens),
	close(Stream).

html_structure(Title,Styles,Js,Metas) -->
		page([title([Title])],
			[ h2(align(center),
                  [Title]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Style tags')
                          ])
                     |\print_styles(Styles)
                     ]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('JS tags')
                          ])
                     |\print_styles(Js)
                     ]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Meta tags')
                          ])
                     |\print_styles(Metas)
                     ])
             ]).

print_styles([]) -->
        [].
print_styles([X|Xs]) -->
        html([ tr([ td(X)
                  ])
             ]),
        print_styles(Xs).

create_rows([]) -->
        [].
create_rows([X|Xs]) -->
        html([ tr([ td(X)
                  ])
             ]),
        create_rows(Xs).

html_apropos(Kwd) :-
        findall(Pred, apropos_predicate(Kwd, Pred), Matches),
        phrase(apropos_page(Kwd, Matches), Tokens),
        print_html(Tokens).

%       emit page with title, header and table of matches

apropos_page(Kwd, Matches) -->
        page([ title(['Predicates for ', Kwd])
             ],
             [ h2(align(center),
                  ['Predicates for ', Kwd]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Predicate'),
                            th('Summary')
                          ])
                     |\apropos_rows(Matches)
                     ])
             ]).

%       emit the rows for the body of the table.

apropos_rows([]) -->
        [].
apropos_rows([pred(Name, Arity, Summary)|T]) -->
        html([ tr([ td(\predref(Name/Arity)),
                    td(em(Summary))
                  ])
             ]),
        apropos_rows(T).

%       predref(Name/Arity)
%
%       Emit Name/Arity as a hyperlink to
%
%               /cgi-bin/plman?name=Name&arity=Arity
%
%       we must do form-encoding for the name as it may contain illegal
%       characters.  www_form_encode/2 is defined in library(url).

predref(Name/Arity) -->
        { www_form_encode(Name, Encoded),
          sformat(Href, '/cgi-bin/plman?name=~w&arity=~w',
                  [Encoded, Arity])
        },
        html(a(href(Href), [Name, /, Arity])).

%       Find predicates from a keyword. '$apropos_match' is an internal
%       undocumented predicate.

apropos_predicate(Pattern, pred(Name, Arity, Summary)) :-
        predicate(Name, Arity, Summary, _, _),
        (   '$apropos_match'(Pattern, Name)
        ->  true
        ;   '$apropos_match'(Pattern, Summary)
        ).

% Test predicates
test1 :- html_create_document('index.html','Test doc',['s1',s2,'s3'],['js1'],[m1:'m11','m2']).






































































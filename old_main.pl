:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(url)).
:- use_module(library(http/html_write)).

%-------------------%
% STRING PREDICATES %
%-------------------%

% Test whether an URL ends with the given pattern
% Param: the URL to be tested
% Param: the pattern to be applied

endsWith(URL, Pattern) :- sub_string(URL,_,_,0,Pattern).

% Test whether an URL starts with the given pattern
% Param: the URL to be tested
% Param: the pattern to be applied

startsWith(URL, Pattern) :- sub_string(URL,0,_,_,Pattern).

% Test whether an URL contains the given pattern
% Param: the URL to be tested
% Param: the pattern to be applied

containsPattern(URL, Pattern) :- sub_string(URL, _, _, _, Pattern).

% Test whether an URL starts with 'http://'
% Param: the URL to be tested

startsWithHttp(URL) :- sub_string(URL,0,_,_,'http://').

% Test whether an URL starts with 'https://'
% Param: the URL to be tested

startsWithHttps(URL) :- sub_string(URL,0,_,_,'https://').

% Extract host name from the given URL. It uses the aux.
% predicate get_host
% Param: the URL from which the host will be extracted
% Param: the resulting host name

extract_host_name(URL,Host) :-
		parse_url(URL, Attr),
		get_host(Attr,Host),!.
extract_host_name(URL,URL).

% This predicate gets the host name from the URL attributes
% retrieved with URL library
% Param: list of URL attributes
% Param: host name extracted from those attributes

get_host([],_) :- fail.
get_host([host(H) | _],H) :- !.
get_host([_ | Ls], H) :- get_host(Ls, H).

% DCG and two predicates to replace characters. 
% It will be mainly used to obtain the file names
% from the retrieved URLs.
% Param: pattern to be replaced
% Param: replacing pattern
% Param: initial string 
% Param: resulting string

substitute(Find, Replace, Request, Result) :-
        phrase(replace(Find, Replace), Request, Result).

replace(_, _) --> call(eos), !.
replace(Find, Replace), Replace -->
        Find,
        !,
        replace(Find, Replace).
replace(Find, Replace), [C] -->
        [C],
        replace(Find, Replace).

eos([], []).

%------------------%
% LINKS PREDICATES %
%------------------%

% Test whether an URL is a valid link to be processed. Actually we
% only support HTTP (not SSL) connections.
% Param: the URL to be tested

is_valid_link(URL) :- startsWithHttp(URL),
	% We only support exploring html valid formats
	(
	    is_valid_output(URL);
	    is_valid_host(URL)
	).

% Test if an URL is a valid HTML content output. It only
% checks for the most usual endings.
% Param: the URL to be tested

is_valid_output(URL) :-
	    endsWith(URL,'/');
	    endsWith(URL,'.html');
	    endsWith(URL,'.htm');
	    endsWith(URL,'.ihtml');
	    endsWith(URL,'.ghtml');
	    endsWith(URL,'.phtml');
	    endsWith(URL,'.shtml');
	    endsWith(URL,'.asp');
	    endsWith(URL,'.jsp');
	    endsWith(URL,'.pl');
	    endsWith(URL,'.php');
	    endsWith(URL,'.cfm');
	    endsWith(URL,'.xml').

% Test if an URL is a valid domain name (without index.xxxx)
% We will use the host name to validate the URL
% Param: the URL to be tested

is_valid_host(URL) :-
		extract_host_name(URL, Host),
		endsWith(URL, Host).

% Get all link labels from a DOM structure. It is actually unused
% Param: the HTML DOM structure
% Param: the output link labels

get_link_labels(DOM, HREF):-
	% Using XPath expression to retrieve links list
	xpath(DOM,//a(@href),HREF).

% Get all links list
% Param: the HTML DOM structure
% Param: the output link list

get_all_link_list(DOM, List) :-
	setof(L, xpath(DOM,//a(@href),L), List).

% Get all HTTP or HTTPS links (in list form)
% Param: the HTML DOM structure
% Param: the output link list

get_link_list(DOM, List) :-
	setof(L,
	      (xpath(DOM,//a(@href),L),
	      (startsWithHttp(L);startsWithHttps(L))),
	      List),!.
% Clause needed to avoid retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_link_list(_,[]).

% Get only valid links. This predicates avoid scanning already
% visited links.
% Param: the actual web URL list
% Param: input list with the visited links until now
% Param: output list with the new valid links to visit

get_valid_links([],_,[]).
get_valid_links([X|Xs], VisitedLinks, [X|Ys]) :-
	\+member(X, VisitedLinks),
	is_valid_link(X),!,
	append(VisitedLinks, [X], NewVisitedLinks),
	get_valid_links(Xs, NewVisitedLinks, Ys).
get_valid_links([_|Xs], VisitedLinks, Ys) :-
	get_valid_links(Xs, VisitedLinks, Ys).

% This predicate tests if the given URL passes all user filters. It will
% be used when the user wants -c,-s,-e params
% Param: URL to be tested
% Param: pattern to be tested as starting of the URL
% Param: pattern to be tested if it is contained in the URL
% Param: patter to be tested as ending of the URL

filter_url(URL, Starts, Contains, Ends) :-
	startsWith(URL, Starts),
	containsPattern(URL, Contains),
	endsWith(URL, Ends).

% Filter retrieved links with the given patterns. It uses the aux. predicate
% filter_url.
% Param: link list to be checked
% Param: link list of already taken URLs. It can be used to avoid scanning
%        already visited URLs.
% Param: pattern to be tested as starting of each URL
% Param: pattern to be tested if it is contained in each URL
% Param: patter to be tested as ending of each URL
% Param: resulting filtered link list

get_filtered_links([], _, _, _, _, []).
get_filtered_links([X|CheckLinks], TakenLinks, Starts, Contains, Ends, [X|Ys]) :-
	\+member(X, TakenLinks),
    filter_url(X, Starts, Contains, Ends),!,
	append(TakenLinks, [X], NewTaken),
	get_filtered_links(CheckLinks, NewTaken, Starts, Contains, Ends, Ys).
get_filtered_links([_|CheckLinks], TakenLinks, Starts, Contains, Ends, NewLinks) :-
	get_filtered_links(CheckLinks, TakenLinks, Starts, Contains, Ends, NewLinks).

%----------------%
% CSS PREDICATES %
%----------------%

% Get all stylesheet links list
% Param: the HTML DOM structure from which the style links will be retrieved
% Param: retrieved style links list

get_all_style_list(DOM, List) :-
	setof(L, xpath(DOM,//link(@rel='stylesheet',@href),L), List),!.
% Clause needed to avoid style retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_style_list(_,[]).

% This predicate tests if there is some style in the given HTML.
% It is neccesary because a style section may not be linked via <link>
% label
% Param: the HTML DOM structure for the search

uses_style(DOM) :- xpath(DOM,//style,_);
                   xpath(DOM,//link(@type='text/css'),_).

%---------------%
% JS PREDICATES %
%---------------%

% Get all Javascript links list
% Param: the HTML DOM structure
% Param: the JS links retrieved list

get_all_js_list(DOM, List) :-
	setof(L,
	      xpath(DOM,//script(@type='text/javascript',@src),L),
	      List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_js_list(_,[]).

% This predicate tests if there is some JS script in the given DOM.
% It is neccesary because a JS script may not be linked via <script>
% label
% Param: the HTML DOM structure

uses_js(DOM) :- xpath(DOM,//script(@type='text/javascript'),_).

% Predicate to init the JS file for the graph
% Param: the main output folder (without the /js/ ending)
% Param: the new JS file name
% Param: output stream generated with the given params

init_graph_js(Folder, FileName, Stream) :-
	% Compose JS file path
	append(Folder,"/js/",F1),
	append(F1, FileName, F2),
	append(F2, ".js", JsFile),
	name(JsFilePath, JsFile),
	% Open file
	write('Generating JS file: '),writeln(JsFilePath),
	open(JsFilePath, write, Stream, [encoding(utf8)]),
	% Init file
	cleanly_write(Stream, '$(document).ready(function() {'), nl(Stream),
	cleanly_write(Stream, 'var width = $(window).width();'), nl(Stream),
	cleanly_write(Stream, 'var height = $(window).height();'), nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
init_graph_js(_, _, _) :-
	write('Error: cannot create the JS file. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to generate one graph head
% Param: initialized output stream where to dump all the JS code
% Param: graph name. It will be used to generate all needed JS vars

head_graph_js(Stream, Name) :-
	name(Name, N),
	append("var g", N, H1),
	append(H1, " = new Graph();", H2),
	name(GraphHead, H2),
	cleanly_write(Stream, GraphHead),
	nl(Stream),
	append("g", N, H3),
	append(H3, ".edgeFactory.template.style.directed = true;", H4),
	name(GraphDirected, H4),
	cleanly_write(Stream, GraphDirected),
	nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
head_graph_js(_, _) :-
	write('Error: cannot create the graph head. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to generate one graph ending
% Param: initialized output stream where to dump all the JS code
% Param: graph name. It will be used to generate all needed JS code

ending_graph_js(Stream, Name) :-
	name(Name, N),
	append("var layouter", N, E1),
	append(E1, " = new Graph.Layout.Spring(g", E2),
	append(E2, N, E3),
	append(E3, ");", E4),
	name(Layouter, E4),
	cleanly_write(Stream, Layouter),
	nl(Stream),
	append("var renderer", N, E21),
	append(E21, " = new Graph.Renderer.Raphael('canvas", E22),
	append(E22, N, E23),
	append(E23, "', g", E24),
	append(E24, N, E25),
	append(E25, ", width, height);", E26),
	name(Renderer, E26),
	cleanly_write(Stream, Renderer),
	nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
ending_graph_js(_, _) :-
	write('Error: cannot create the graph ending. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to close the JS file for the graph
% Param: initialized output stream where to be closed

close_graph_js(Stream) :-
	cleanly_write(Stream, '});'),
	% Close file
	close(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
close_graph_js(_) :-
	write('Error: cannot create the JS file. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to dump the complete graph into the JS file.
% Param: list of graph edges
% Param: initialized output stream
% Param: graph name to be used in JS code

full_js_graph([], _, _).
full_js_graph([V1-V2|Xs], Stream, Name) :-
		name(Name, N1),
		append("g", N1, N2),
		append(N2, ".addEdge(\"", N3),
		name(GStart, N3),
		cleanly_write(Stream, GStart),
		cleanly_write(Stream, V1),
		cleanly_write(Stream, '" , "'),
		cleanly_write(Stream, V2),
		cleanly_write(Stream, '");'),
		nl(Stream),
		!,
		full_js_graph(Xs, Stream, Name).
% Continue dump althought one step fails
full_js_graph([_|Xs], Stream, Name) :-
		full_js_graph(Xs, Stream, Name),!.

% Predicate to dump one graph section into the JS file
% Param: the root node
% Param: list of connected nodes to the given root
% Param: initialized output stream
% Param: graph name to be used in JS code

partial_js_graph(_, [], _, _).
partial_js_graph(Root, [X|Xs], Stream, Name) :-
		name(Name, N1),
		append("g", N1, N2),
		append(N2, ".addEdge(\"", N3),
		name(GStart, N3),
		cleanly_write(Stream, GStart),
		cleanly_write(Stream, Root),
		cleanly_write(Stream, '" , "'),
		cleanly_write(Stream, X),
		cleanly_write(Stream, '");'),
		nl(Stream),
		!,
		partial_js_graph(Root, Xs, Stream, Name).
% Continue dump althought one step fails
partial_js_graph(Root, [_|Xs], Stream, Name) :-
		partial_js_graph(Root, Xs, Stream, Name),!.

%-----------------%
% META PREDICATES %
%-----------------%

% Get all meta elems in the given HTML (list form)
% Param: the HTML DOM structure
% Param: the retrieved meta list

get_all_meta_list(DOM, List) :-
	setof(L, xpath(DOM,//meta,L), List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_meta_list(_, []).

% Get HTML5 charset metatag
% Param: list of metas where to search
% Param: the HTML5 charset tag or not found string

get_html_charset([],'No defined HTML5 charset tag').
get_html_charset([M|_], Charset) :-
	xpath(M,//meta(@charset),Charset),!.
get_html_charset([_|MetaTags],Charset) :-
	get_html_charset(MetaTags,Charset).

% Get all content metatags. They will be grouped into pairs with the
% form ContentType-ContentValue
% Param: list of metas where to perform the search
% Param: list of retrieved content and http-equiv metas in pairs key:value

get_all_content_meta([], []).
get_all_content_meta([M|MetaTags], [X:Y|CMetas]) :-
	xpath(M,//meta(@name),X),
	xpath(M,//meta(@content),Y),!,
	get_all_content_meta(MetaTags,CMetas).
get_all_content_meta([M|MetaTags], [X:Y|CMetas]) :-
	xpath(M,//meta(@'http-equiv'),X),
	xpath(M,//meta(@content),Y),!,
	get_all_content_meta(MetaTags,CMetas).
get_all_content_meta([_|MetaTags], CMetas) :-
	get_all_content_meta(MetaTags, CMetas).

%-----------------%
% HTML PREDICATES %
%-----------------%

% Load an HTML given its URL. At the moment only HTTP connections work properly.
% SSL support will be added later (if possible).
% Param: input URL to load
% Param: output HTML DOM structure

load_html(URL, DOM) :-
	catch(
	% Try section
	    (	setup_call_cleanup(http_open(URL, In,
				    [ timeout(60)
				    ]),
					( dtd(html, DTD),
					load_structure(stream(In),
					DOM,
					[ dtd(DTD),
					    % CHANGED: dialect(sgml)
						dialect(xml),
						shorttag(false),
						max_errors(-1),
						syntax_errors(quiet)
					])
					),
					close(In))),
	 % Exception
	    _,
	 % Catch section. Don't dump exception later. Give an
	 % error info message instead
	    ( fail )
	    ).

% This predicate is needed in order to avoid some html reading errors
% and continue executing the crawler
% Param: input URL to load
% Param: output HTML DOM structure

cleanly_load_html(URL,DOM) :-
	load_html(URL,DOM),!.
cleanly_load_html(_,[]).

% This predicate creates an HTML output document and dumps
% all retrieved data (finding crawler)
% Param: path where the new output file will be created
% Param: main title for the output HTML (in this case, the base URL)
% Param: pattern with the user starting param
% Param: pattern with the user contains param
% Param: pattern with the user ending param
% Param: retrieved links (already filtered)
% Param: finding depth

f_html_create_document(URI,Title,Starts,Contains,Ends,Links,Depth) :-
	phrase(f_html_structure(Title,Starts,Contains,Ends,Links,Depth), Tokens),
	open(URI, write, Stream, [encoding(utf8)]),
	print_html(Stream,Tokens),
	close(Stream).

% Predicate to generate the HTML structure to be dumped
% Param: main title for the output HTML (in this case, the base URL)
% Param: pattern with the user starting param
% Param: pattern with the user contains param
% Param: pattern with the user ending param
% Param: retrieved links (already filtered)
% Param: finding depth

f_html_structure(Title,Starts,Contains,Ends,Links,Depth) -->
		page([title(['URL filter']),
			meta(['http-equiv'('content-type'),content('text/html; charset=utf-8')]),
			link([rel('stylesheet'),type('text/css'),href('css/main.css')])
			],
			[ h2(align(center),
                        [Title]
		       ),
               table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th([colspan(2)],
						'Filters'
						)
                          ]),
				       tr([ td([width('50%')],
					       'Root URL:'),
					    	td([width('50%')],
					       Title)
		                          ]),
				       tr([ td([width('50%')],
					       'Depth:'),
					    	td([width('50%')],
					       Depth)
		                          ]),
				       tr([ td([width('50%')],
					       'URLs starting with:'),
					    	td([width('50%')],
					       Starts)
		                          ]),
				       tr([ td([width('50%')],
					       'URLs containing:'),
					    	td([width('50%')],
					       Contains)
		                          ]),
				       tr([ td([width('50%')],
					       'URLs ending with:'),
					    	td([width('50%')],
					       Ends)
		                          ])
                     ]),
	        table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th('Retrieved links')
                          ])
                     |\create_linked_rows(Title, Links)
                     ])
             ]).

% This predicate creates an HTML output document and dumps
% all retrieved data (retrieving crawler)
% Param: path where the new output file will be created
% Param: main title for the output HTML (in this case, the base URL)
% Param: retrieved HTML5 charset
% Param: all style links list
% Param: all JS links list
% Param: all meta list
% Param: hosts graph
% Param: complete URLs graph

html_create_document(URI,Title,Charset,Styles,Js,Metas,Graph,CompleteGraph) :-
	phrase(html_structure(Title,Charset,Styles,Js,Metas,Graph,CompleteGraph), Tokens),
	open(URI, write, Stream, [encoding(utf8)]),
	print_html(Stream,Tokens),
	close(Stream).

% Predicate to generate the HTML structure to be dumped
% Param: main title for the output HTML (in this case, the base URL)
% Param: retrieved HTML5 charset
% Param: all style links list
% Param: all JS links list
% Param: all meta list
% Param: hosts graph
% Param: complete URLs graph

html_structure(Title,Charset,Styles,Js,Metas,Graph,CompleteGraph) -->
		page([title([Title]),
			meta(['http-equiv'('content-type'),content('text/html; charset=utf-8')]),
			link([rel('stylesheet'),type('text/css'),href('css/main.css')])
			],
			[ h2(align(center),
                        [Title]
		       ),
			   table([ align(center),
                       width('100%')
                     ],
                     [
			tr([ th('Index') ])
			|\create_index
                     ]),
		   table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th(
							a([name('Charset')],'Charset')
						)
                          ]),
		       tr([ td(Charset)
                          ])
                     ]),
               table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th(
							a([name('Style')],'Style tags')
						)
                          ])
                     |\create_linked_rows(Title,Styles)
                     ]),
               table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th(
							a([name('JS')],'JavaScript tags')
						)
                          ])
                     |\create_linked_rows(Title,Js)
                     ]),
               table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th([colspan(2)],
						a([name('Meta')],'Meta tags')
						)
                          ]),
		       tr([ th([width('50%')],
			       'Type'),
			    th([width('50%')],
			       'Content')
                          ])
                     |\create_meta_rows(Metas)
                     ]),
	        table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th(
							a([name('HostsTable')],'Hosts graph (table)')
						)
                          ])
                     |\dump_complete_graph(Graph, Graph, 0)
                     ]),
	        table([ align(center),
                       width('100%')
                     ],
                     [ tr([ th(
							a([name('LinksTable')],'Links table')
						)
                          ])
                     |\dump_complete_graph(CompleteGraph, CompleteGraph, 1)
                     ])
             ]).

% Predicate to split the given graph in multiple HTML docs
% Param: starting root URL
% Param: retrieved graph
% Param: main output folder (without /graphs/)

generate_d_graph_html(URL, Graph, Folder) :-
	vertices(Graph, V),
	% Generate a single document only if there are
	% less than N elements
	length(V, L),
	L < 20,!,
	%append(Folder, "/graphs", GDir),	
	%append(Directory,"graph.html",GraphURI),
	%name(GURI,GraphURI),
	append(Folder, "/graph1.html", GraphURI),
	name(GURI, GraphURI),
	write('Generating graph file: '),writeln(GURI),
	html_create_graph_document(GURI, "1", URL, Graph, Folder).

generate_d_graph_html(URL, Graph, Folder) :-
	!,
	%append(Folder, "/graph1.html", GraphURI),
	%name(GURI, GraphURI),
	%html_create_graph_document(GURI, "1", URL, Graph, Folder).
	filter_graph(Graph, FGraph),
	separate_graph(FGraph, SepGraph),
	write_separated_graph(SepGraph, URL, Folder, 1).

% Predicate to help writing the bigger graphs in multiple html docs
% Param: retrieved s-graph
% Param: starting root URL
% Param: main output folder (without /graphs/)
% Param: actual graph ending. It must be an integer. It will be used to
%        generate all output files name

write_separated_graph([], _, _, _) :- !.
write_separated_graph([G|Gs], URL, Folder, Ending) :- 
	!,	
	name(Ending, EList),
	append(Folder, "/graph", A1),
	append(A1, EList, A2),
	append(A2, ".html", GraphURI),
	name(GURI, GraphURI),
	write('Generating graph file: '),writeln(GURI),
	html_create_graph_document(GURI, EList, URL, G, Folder),
	NewEnding is Ending + 1,
	write_separated_graph(Gs, URL, Folder, NewEnding).

% Filter graph to avoid writing empty subgraphs.
% Param: starting graph
% Param: resulting filtered graph

filter_graph([], []).
filter_graph([_-[]|Xs], FilteredG) :-
	!,
	filter_graph(Xs, FilteredG).
filter_graph([X|Xs], FilteredG) :-
	!,
	filter_graph(Xs, FG1),
	append([X], FG1, FilteredG).

% Predicate to separate the given graph in sections of N to N elems
% Param: initial graph
% Param: resulting splitted graph

separate_graph([], []) :- !.
separate_graph(Graph, Result) :-
	!,
	take_n(Graph, Remaining, Taken, 0),
	separate_graph(Remaining, E1),
	append([Taken], E1, Result).
	
% Predicate to take N elems from a given list. In this case 20. We don't
% parametrize this predicate as we only need 20 elems
% Param: initial list
% Param: remaining list
% Param: list with the taken elements
% Param: number of elements taken until now

take_n([], [], [], _) :- !.
take_n(Remaining, Remaining, [], 20) :- !.
take_n([X|Xs], Remaining, Taken, N) :- 
	!,
	M is N+1,
	take_n(Xs, Remaining, TAux, M),
	append([X], TAux, Taken).

% This predicate creates an HTML output document and dumps
% the hosts graph
% Param: path for the actual HTML graph doc
% Param: actual JS ending (due to multiple graphs)
% Param: HTML title (in this case, the root URL)
% Param: hosts graph
% Param: main output folder

html_create_graph_document(URI,JSEnd,Title,Graph,Folder) :-
	phrase(html_graph_structure(Title,JSEnd,Graph,Folder), Tokens),
	open(URI, write, Stream, [encoding(utf8)]),
	print_html(Stream,Tokens),
	close(Stream).

% DCG to generate the output HTML graphs structure
% Param: HTML title (in this case, the root URL)
% Param: actual JS ending (due to multiple graphs)
% Param: hosts graph
% Param: main output folder

html_graph_structure(Title,JSEnd,Graph,Folder) -->
		{
			append("js/index", JSEnd, A1),
			append(A1, ".js", A2),
			name(JSFile, A2)
		},
		page([title(['Hosts graph']),
			meta(['http-equiv'('content-type'),content('text/html; charset=utf-8')]),
			link([rel('stylesheet'),type('text/css'),href('css/main.css')]),
			script([type('text/javascript'),src('js/raphael-min.js')],''),
			script([type('text/javascript'),src('js/dracula_graffle.js')],''),
			script([type('text/javascript'),src('js/jquery-1.4.2.min.js')],''),
			script([type('text/javascript'),src('js/dracula_graph.js')],''),
			script([type('text/javascript'),src('js/dracula_algorithms.js')],''),
			script([type('text/javascript'),src(JSFile)],'')
			],
			[
			   \dump_graph_to_html(Title,JSEnd,Graph,Folder)
            ]).

% Predicate to dump the graph HTML content
% Param: HTML title (in this case, the root URL)
% Param: actual JS ending (due to multiple graphs)
% Param: hosts graph
% Param: main output folder

dump_graph_to_html(Title,JSEnd,Graph,Folder) -->
	{edges(Graph, V),
	length(V, L),
	% Check if the complete graph is small enought
	L < 50, !},
	dump_one_graph(Title,JSEnd,Graph,Folder).

dump_graph_to_html(_,JSEnd,Graph,Folder) -->
	{
		append("index", JSEnd, JSName),
		init_graph_js(Folder, JSName, Stream)
	},
	dump_multiple_graphs(Graph,Stream,0),
	{
		close_graph_js(Stream),
		!
	}.

dump_graph_to_html(_,_,_,_) -->
	{writeln('Error: graph has not been created. Aborting execution.')},
	[].

% Generate one graph JS
% Param: HTML title (in this case, the root URL)
% Param: actual JS ending (due to multiple graphs)
% Param: hosts graph
% Param: main output folder

dump_one_graph(Title,JSEnd,Graph,Folder) -->
	{
		edges(Graph, Edges),
		append("index", JSEnd, JSName),
		init_graph_js(Folder, JSName, Stream),
		head_graph_js(Stream, 1),
		full_js_graph(Edges, Stream, 1),
		ending_graph_js(Stream, 1),
		close_graph_js(Stream),
		!
	},
	html([
		h2(align(center),
			[Title]
			),
		div([id('canvas1')],'')
	]).
dump_one_graph(_,_,_) --> [].

% Generate multiple graphs JS
% Param: graph to be dumped
% Param: initialized output stream
% Param: graph name (to be used in JS code)

dump_multiple_graphs([],_,_) --> [].

% Don't dump empty roots
dump_multiple_graphs([_-[]|Xs],Stream,Name) -->
	{!},
	dump_multiple_graphs(Xs,Stream,Name).

dump_multiple_graphs([Root-Nodes|Xs],Stream,Name) -->
	{
		head_graph_js(Stream, Name),
		length(Nodes, L),
		(
			(L > 30) ->
			partial_js_graph(Root, ['Too much nodes to be displayed'], Stream, Name)
			;
			partial_js_graph(Root, Nodes, Stream, Name)
		),
		ending_graph_js(Stream, Name),
		name(Name, N1),
		append("canvas", N1, C1),
		name(CanvasName, C1),
		NextName is Name + 1,
		!
	},
	html([
		h2(align(center),
			[Root]
			),
		div([id(CanvasName),class('g_canvas')],'')
	]),
	dump_multiple_graphs(Xs,Stream,NextName).

dump_multiple_graphs([_|Xs],Stream,Name) -->
	{!},
	dump_multiple_graphs(Xs,Stream,Name).

% DCG to create the output HTML index

create_index -->
	    {name(CCharset,"#Charset")},
	    {name(CStyle,"#Style")},
	{name(CJS,"#JS")},
	{name(CMeta,"#Meta")},
	{name(CHTable,"#HostsTable")},
	{name(CLTable,"#LinksTable")},
		html([
		%tr([ td(
		%		a([href('graph.html'),target('_blank')],'Hosts graph')
		%	)
		%]),
		tr([ td(
				a([href(CCharset)],'Charset')
			)
		]),
		tr([ td(
				a([href(CStyle)],'Style')
			)
		]),
		tr([ td(
				a([href(CJS)],'JavaScript')
			)
		]),
		tr([ td(
				a([href(CMeta)],'Metas')
			)
		]),
		tr([ td(
				a([href(CHTable)],'Hosts table')
			)
		]),
		tr([ td(
				a([href(CLTable)],'Links table')
			)
		])
		]).

% Create all the HTML rows structure based on the given tags list
% Param: list containing the rows elements

create_rows([]) -->
        [].
create_rows([X|Xs]) -->
        html([ tr([ td(X)
                  ])
             ]),
        create_rows(Xs).

% Create all the HTML rows structure based on the given tags list.
% The difference between upper predicate is that it generates links to
% the real contents
% Param: base url to compose and link to the complete path of every resource
% Param: list of row elements

create_linked_rows(_, []) -->
        [].
create_linked_rows(BaseUrl, [X|Xs]) -->
		% Generate link URL
		{
			catch(
				global_url(X, BaseUrl, GLink),
				_,
				fail
			)
		},
        html([ tr([
				td(
					a([href(GLink),target('_blank')],
						X
					  )
					)
                  ])
             ]),!,
        create_linked_rows(BaseUrl, Xs).
% Clause needed in case one of the links fail
create_linked_rows(BaseUrl, [X|Xs]) -->
        html([ tr([ td(X)
                  ])
             ]),!,
        create_linked_rows(BaseUrl, Xs).

% DCG for meta tags rows
% Param: list of metas in key:value format

create_meta_rows([]) -->
        [].
create_meta_rows([T1:C1|Xs]) -->
        html([ tr([ td(T1)
                   ,
		    td(C1)]
		  )
             ]),
        create_meta_rows(Xs).

% DCG to dump the complete graph in text form
% Param: complete graph to be dumped
% Param: the complete graph. Needed to check for headed nodes
% Param: this params indicates whether to write or not external links

dump_complete_graph([], _, _) -->
		[].
% Do not write isolated nodes
dump_complete_graph([_-[]|Xs], Graph, External) -->
		!,
		dump_complete_graph(Xs, Graph, External).
dump_complete_graph([Ver-Neigh|Xs], Graph, External) -->
		html([ tr([
					th(
						a([name(Ver)],
							Ver
							)
						)
	               ])
	          ]),!,
		generate_link_neigh(Neigh, Graph, External),
		dump_complete_graph(Xs, Graph, External).
% Continue dump althought one step fails
dump_complete_graph([_|Xs], Graph, External) -->
		dump_complete_graph(Xs, Graph, External),!.

% Aux. predicate to dump every link neighbour. "External" param will be used to
% write or not external links.
% Param: list of neighbours from one graph node
% Param: complete graph. Needed to check for headed nodes
% Param: this param indicates whether to write or not external links

generate_link_neigh([], _, _) -->
		[].
generate_link_neigh([N|Xs], Graph, External) -->
		{
		not_headed_node(Graph, N),
		( (External =:= 1) -> DLink =  a([href(N)],N) ; DLink = N )
		},
		html([ tr([
				   td(DLink)
	               ])
	          ]),
		!,
		generate_link_neigh(Xs, Graph, External).
generate_link_neigh([N|Xs], Graph, External) -->
		% Generate anchor
		{
		name(N,L1),
		append("#",L1,L2),
		name(Anchor,L2),
		( (External =:= 1) -> DLink = a([href(N)],N) ; DLink = N )
		},
		html([ tr([
			    td(
			         [DLink,
					  '  -->  ',
				      a([href(Anchor)],
				        '(Go to anchor)'
				      )]
			       )
	            ])
	        ]),
		!,
		generate_link_neigh(Xs, Graph, External).
% Continue dump althought one step fails
generate_link_neigh([_|Xs], Graph, External) -->
		generate_link_neigh(Xs, Graph, External),!.

% Predicate to test if the given node will be on
% the graph headers (used for anchors)
% Param: complete graph
% Param: node to be tested

not_headed_node([N-[]|_], N).
not_headed_node([_|Xs], N) :-
	not_headed_node(Xs, N).

%---------------------------%
% FILES & FOLDER PREDICATES %
%---------------------------%

% Predicate to cleanly write the given text to the output stream
% Param: initialized output stream
% Param: text to be written

cleanly_write(Stream, Text) :-
	atom_chars(Text, CharList),
	dump_to_stream(CharList, Stream).
	
% Dump the given char list into Stream
% Param: char list to be dumped
% Param: initialized output stream

dump_to_stream([], _).
dump_to_stream([X|Xs], Stream) :-
	put(Stream, X),
	dump_to_stream(Xs, Stream),!.
% In case of fail, we avoid dumping that char. This is not the cleanest approach but
% its needed for avoid stopping the application
dump_to_stream([_|Xs], Stream) :-
	dump_to_stream(Xs, Stream).

% Predicate to copy one file to another. It will be used to
% auto copy the CSS file to every output folder
% Param: origin file path
% Param: destination file path

copy(File1, File2) :-
	write('Copying file: from '),write(File1),write(' to '),writeln(File2),
	open(File1,read,Stream1, [encoding(utf8)]),
	open(File2,write,Stream2, [encoding(utf8)]),
	copy_stream_data(Stream1,Stream2),
	close(Stream1),
	close(Stream2),!.
% Clause needed to avoid problems. If CSS is not copied the system
% can continue running without problems.
copy(_, _).

% Predicate to create the ouput folder (scanning crawler)
% Param: output main folder
% Param: output content folder
% Param: output graphs folder

create_dump_folder(Folder, ContentFolder, GraphsFolder) :-
	get_time(TimeStamp),
	stamp_date_time(TimeStamp,LocalDate,local),
	LocalDate = date(Y, M, D, H, Min, Sec, _, _, _),
	name(Y, Year),
	append(Year, "-", A1),
	name(M, Month),
	append(A1, Month, A2),
	append(A2, "-", A3),
	name(D, Day),
	append(A3, Day, A4),
	append(A4, " ", A5),
	name(H, Hour),
	append(A5, Hour, A6),
	append(A6, "-", A7),
	name(Min, Minutes),
	append(A7, Minutes, A8),
	append(A8, "-", A9),
	name(Sec, Seconds),
	append(A9, Seconds, Folder),
	name(FNm, Folder),
	write('Creating folder: '),writeln(FNm),
	make_directory(Folder),
	% Create folder to dump all secondary web data
	append(Folder, "/other_data",ContentFolder),
	name(FNm2, ContentFolder),
	write('Creating folder: '),writeln(FNm2),
	make_directory(ContentFolder),
	% Create folder to dump all graphs
	append(Folder, "/graphs",GraphsFolder),
	name(FNm3, GraphsFolder),
	write('Creating folder: '),writeln(FNm3),
	make_directory(GraphsFolder),
	!.
create_dump_folder(_, _, _) :-
	write('Error: cannot create the output folder. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to create the ouput folder (finding crawler)
% Param: output main folder

f_create_dump_folder(Folder) :-
	get_time(TimeStamp),
	stamp_date_time(TimeStamp,LocalDate,local),
	LocalDate = date(Y, M, D, H, Min, Sec, _, _, _),
	name(Y, Year),
	append(Year, "-", A1),
	name(M, Month),
	append(A1, Month, A2),
	append(A2, "-", A3),
	name(D, Day),
	append(A3, Day, A4),
	append(A4, " ", A5),
	name(H, Hour),
	append(A5, Hour, A6),
	append(A6, "-", A7),
	name(Min, Minutes),
	append(A7, Minutes, A8),
	append(A8, "-", A9),
	name(Sec, Seconds),
	append(A9, Seconds, Folder),
	name(FNm, Folder),
	write('Creating folder: '),writeln(FNm),
	make_directory(Folder),
	!.
f_create_dump_folder(_) :-
	write('Error: cannot create the output folder. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to copy the CSS files to the ouput folder
% Param: output root folder. CSS folder will be generated based on this

generate_css_file(Folder) :-
	append(Folder,"/css",CssDirectory),
	name(FNm, CssDirectory),
	write('Creating folder: '),writeln(FNm),
	make_directory(CssDirectory),
	append(CssDirectory,"/main.css",CssFile),
	name(CssFilePath, CssFile),
	copy('crawler_css/main.css',CssFilePath),!.
generate_css_file(_) :-
	write('Warning: cannot create the css folder. '),
	writeln('Please, check you have got the right permissions.').

% Predicate to copy the JS files to the ouput folder
% Param: output root folder. JS folder will be generated based on this

generate_js_files(Folder) :-
	append(Folder,"/graphs/js",JsDirectory),
	name(FNm, JsDirectory),
	write('Creating folder: '),writeln(FNm),
	make_directory(JsDirectory),
	% First JS file
	append(JsDirectory,"/raphael-min.js",JsFile1),
	name(JsFile1Path, JsFile1),
	copy('crawler_js/raphael-min.js',JsFile1Path),
	% Second JS file
	append(JsDirectory,"/dracula_graffle.js",JsFile2),
	name(JsFile2Path, JsFile2),
	copy('crawler_js/dracula_graffle.js',JsFile2Path),
	% Third JS file
	append(JsDirectory,"/jquery-1.4.2.min.js",JsFile3),
	name(JsFile3Path, JsFile3),
	copy('crawler_js/jquery-1.4.2.min.js',JsFile3Path),
	% Fourth JS file
	append(JsDirectory,"/dracula_graph.js",JsFile4),
	name(JsFile4Path, JsFile4),
	copy('crawler_js/dracula_graph.js',JsFile4Path),
	% Fifth JS file
	append(JsDirectory,"/dracula_algorithms.js",JsFile5),
	name(JsFile5Path, JsFile5),
	copy('crawler_js/dracula_algorithms.js',JsFile5Path),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
generate_js_files(_) :-
	write('Error: cannot create the JS folder. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

%-------------------%
% GRAPHS PREDICATES %
%-------------------%

% Generate a graph in depth with the given params. The entries
% will be reduced to host name. This graph will be used to show
% the graphical hosts relations graph
% Param: root URL
% Param: link list connected to root
% Param: output generated graph

generate_graph(BaseUrl,[],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	% Set graph root (base url)
	add_vertices([],[Host],Graph),!.
generate_graph(BaseUrl,[L|Ls],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	generate_graph(Host,Ls,G1),
	(
	(% Add current URL to graph (needed?)
	add_vertices(G1,[Host],G2),
	% Get host name (if possible)
	extract_host_name(L,LHost),
	add_edges(G2,[Host-LHost],Graph)) ->
	true
	;
	true
	).

% Generate a graph in depth with the given params. In this case we
% won't trim to the host name
% Param: root URL
% Param: link list connected to root
% Param: output generated graph

generate_complete_graph(BaseUrl,[],Graph) :-
	% Set graph root (base url)
	add_vertices([],[BaseUrl],Graph),!.
generate_complete_graph(BaseUrl,[L|Ls],Graph) :-
	generate_complete_graph(BaseUrl,Ls,G1),
	(
	(% Add current URL to graph (needed?)
	add_vertices(G1,[BaseUrl],G2),
	% Add edge
	add_edges(G2,[BaseUrl-L],Graph)) ->
	true
	;
	true
	).

%---------------------------%
%  DATA RETRIEVING CRAWLER  %
%---------------------------%

% Predicate to process the base URL. We need this to apply some
% changes to main URL and create the data dump folder
% Param: starting URL
% Param: scanning depth
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter

process_main_url(URL, 0, _, _, _) :-
	!,
	% Create results folder
	create_dump_folder(Folder, _, GraphsFolder),
	% Copy css file to results and graphs folder
	generate_css_file(Folder),
	generate_css_file(GraphsFolder),
	% Copy js files to results folder
	generate_js_files(Folder),
	% Write process info
	write('Processing main (0): '),writeln(URL),
	% Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	% get_valid_links(DOM, ValidLinks),
	% Get stylesheet links
	get_all_style_list(DOM, CssLinks),
	% Get javascript links
	get_all_js_list(DOM, JSLinks),
	% Get all meta elems
	get_all_meta_list(DOM, MetaElms),
	% Get HTML charset
	get_html_charset(MetaElms, Charset),
	% Get content metas
	get_all_content_meta(MetaElms, CMetas),
	% Generate basic hosts graph
	generate_graph(URL, LinkList, OutGraph),
	% Generate complete links graph
	generate_complete_graph(URL, LinkList, OutCompleteGraph),
	% DEBUG: write retrieved data
	%write('All links ->'),writeln(LinkList),nl,
	%write('Valid links ->'),writeln(ValidLinks),nl,
	%write('Css links ->'),writeln(CssLinks),nl,
	%write('Javascript links ->'),writeln(JSLinks),nl,
	%write('Content meta tags ->'),writeln(CMetas),nl,
	%write('Charset ->'),writeln(Charset),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	%write('Complete graph ->'),writeln(OutCompleteGraph),
	%create_graph_js(OutGraph, Folder, "index"),
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	write('Generating file: '),writeln(URI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph),
	% Dump graph in one/multiple HTML doc(s)
	append(Directory, "graphs", GFolder),
	generate_d_graph_html(URL, OutGraph, GFolder).
	%append(Directory,"graph.html",GraphURI),
	%name(GURI,GraphURI),
	%html_create_graph_document(GURI,URL,OutGraph,Folder).

process_main_url(URL, N, Starts, Contains, Ends) :-
	% Create results folder
	create_dump_folder(Folder, ContentFolder, GraphsFolder),
	% Copy css file to results, extra and graphs folders
	generate_css_file(Folder),
	generate_css_file(ContentFolder),
	generate_css_file(GraphsFolder),
	% Copy js files to results folder
	generate_js_files(Folder),
	% Write process info
	write('Processing main ('),write(N),write('): '),writeln(URL),
    % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, [], ValidLinksAux),
	% Filter future links with user params
	get_filtered_links(ValidLinksAux, [], Starts, Contains, Ends, ValidLinks),
	% Calculate visited links (or being visited)
	append(ValidLinks, [URL], VisitedLinks),
	% Get stylesheet links
	get_all_style_list(DOM, CssLinks),
	% Get javascript links
	get_all_js_list(DOM, JSLinks),
	% Get all meta elems
	get_all_meta_list(DOM, MetaElms),
	% Get HTML charset
	get_html_charset(MetaElms, Charset),
	% Get content metas
	get_all_content_meta(MetaElms, CMetas),
	% Generate basic hosts graph
	generate_graph(URL, LinkList, Graph),
	% Generate complete links graph
	generate_complete_graph(URL, LinkList, CompleteGraph),
	% DEBUG: write retrieved data
	%write('All links ->'),writeln(LinkList),nl,
	%write('Valid links ->'),writeln(ValidLinks),nl,
	%write('Visited links ->'),writeln(VisitedLinks),nl,
	%write('Css links ->'),writeln(CssLinks),nl,
	%write('Javascript links ->'),writeln(JSLinks),nl,
	%write('Content meta tags ->'),writeln(CMetas),nl,
	%write('Charset ->'),writeln(Charset),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	evaluate_level(ValidLinks, M, Graph, OutGraph, CompleteGraph, OutCompleteGraph, ContentFolder, VisitedLinks, Starts, Contains, Ends),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	%write('Complete graph ->'),writeln(OutCompleteGraph),
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	write('Generating file: '),writeln(URI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	%write('Complete graph ->'),writeln(OutCompleteGraph),
	%create_graph_js(OutGraph, Folder, "index"),
	% Dump graph in one/multiple HTML doc(s)
	append(Directory, "graphs", GFolder),
	generate_d_graph_html(URL, OutGraph, GFolder),
	%append(Directory,"graph.html",GraphURI),
	%name(GURI,GraphURI),
	%html_create_graph_document(GURI,URL,OutGraph,Folder),
	!.

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
% Param: starting URL
% Param: scanning depth
% Param: output hosts graph
% Param: output complete graph
% Param: root output folder
% Param: list with the already visited list
% Param: output list with the new visited links
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter

process_url(URL, 0, OutGraph, OutCompleteGraph, Folder, VisitedLinks, VisitedLinks, _, _, _) :-
	% Don't try more
	!,
	% Write process info
	write('Processing (0): '),writeln(URL),
	% Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	% get_valid_links(DOM, ValidLinks),
	% Get stylesheet links
	get_all_style_list(DOM, CssLinks),
	% Get javascript links
	get_all_js_list(DOM, JSLinks),
	% Get all meta elems
	get_all_meta_list(DOM, MetaElms),
	% Get HTML charset
	get_html_charset(MetaElms, Charset),
	% Get content metas
	get_all_content_meta(MetaElms, CMetas),
	% Generate basic hosts graph
	generate_graph(URL, LinkList, OutGraph),
	% Generate complete links graph
	generate_complete_graph(URL, LinkList, OutCompleteGraph),
	% DEBUG: write retrieved data
	%write('All links ->'),writeln(LinkList),nl,
	%write('Valid links ->'),writeln(ValidLinks),nl,
	%write('Css links ->'),writeln(CssLinks),nl,
	%write('Javascript links ->'),writeln(JSLinks),nl,
	%write('Content meta tags ->'),writeln(CMetas),nl,
	%write('Charset ->'),writeln(Charset),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	% HTML output dumping
	name(URL,Charlist),
	append(Charlist,".html",URIAppend),
	substitute(":","_",URIAppend,ReplaceURIList),
	substitute("/","_",ReplaceURIList,URITransform),
    append(Folder,"/",Directory),
	append(Directory,URITransform,DirectoryURI),
	name(URI,DirectoryURI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph).

% Process and URL with depth > 1. In this case we must build
% the links graph and explore the new websites
process_url(URL, N, OutGraph, OutCompleteGraph, Folder, VisitedLinks, NewVisitedLinks, Starts, Contains, Ends) :-
	% Write process info
	write('Processing ('),write(N),write('): '),writeln(URL),
        % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, VisitedLinks, ValidLinksAux),
	% Filter future links with user params
	get_filtered_links(ValidLinksAux, [], Starts, Contains, Ends, ValidLinks),
	% Recalculate visited links (althought they haven't been visited yet)
	append(VisitedLinks, ValidLinks, NewVisitedLinks),
	% Get stylesheet links
	get_all_style_list(DOM, CssLinks),
	% Get javascript links
	get_all_js_list(DOM, JSLinks),
	% Get all meta elems
	get_all_meta_list(DOM, MetaElms),
	% Get HTML charset
	get_html_charset(MetaElms, Charset),
	% Get content metas
	get_all_content_meta(MetaElms, CMetas),
	% Generate basic graph
	generate_graph(URL, LinkList, Graph),
	% Generate complete links graph
	generate_complete_graph(URL, LinkList, CompleteGraph),
	% DEBUG: write retrieved data
	%write('All links ->'),writeln(LinkList),nl,
	%write('Valid links ->'),writeln(ValidLinks),nl,
	%write('Css links ->'),writeln(CssLinks),nl,
	%write('Javascript links ->'),writeln(JSLinks),nl,
	%write('Content meta tags ->'),writeln(CMetas),nl,
	%write('Charset ->'),writeln(Charset),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	evaluate_level(ValidLinks, M, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, NewVisitedLinks, Starts, Contains, Ends),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	% HTML output dumping
	name(URL,Charlist),
	append(Charlist,".html",URIAppend),
	substitute(":","_",URIAppend,ReplaceURIList),
	substitute("/","_",ReplaceURIList,URITransform),
    append(Folder,"/",Directory),
	append(Directory,URITransform,DirectoryURI),
	name(URI,DirectoryURI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph),
	!.

% Evaluate remaining levels. We must take care of timeout or redirects
% to HTTPS, so we will take all exceptions and avoid processing the
% associated webs
% Param: list with the remaining links for this level
% Param: scanning deph
% Param: old hosts graph
% Param: output new hosts graph
% Param: old complete graph
% Param: output new complete graph
% Param: root folder
% Param: list with the visited links
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter

evaluate_level([], _ , Graph, Graph, CompleteGraph, CompleteGraph, _, _, _, _, _).
evaluate_level([L|Ls], N, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, VisitedLinks, Starts, Contains, Ends) :-
	catch(
	      % Try section
	      (
	      	  (
	          (process_url(L, N, LevelGraph, LevelCompleteGraph, Folder, VisitedLinks, NewVisitedLinks, Starts, Contains, Ends),
	          % Graphs union
	          ugraph_union(Graph, LevelGraph, OGraph1),
	          ugraph_union(CompleteGraph, LevelCompleteGraph, LCGraph1),
			  evaluate_level(Ls, N, OGraph1, OutGraph, LCGraph1, OutCompleteGraph, Folder, NewVisitedLinks, Starts, Contains, Ends)) ->
			  true
			  ;
			  evaluate_level(Ls, N, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, VisitedLinks, Starts, Contains, Ends)
	      	  )
	      ),
	      % Exception taking
	      _,
	      % Catch section
	      (
		  evaluate_level(Ls, N, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, VisitedLinks, Starts, Contains, Ends))
	      ).

%------------------------%
%  DATA FINDING CRAWLER  %
%------------------------%

% Predicate to process the base URL. We need this to apply some
% changes to main URL and create the data dump folder
% Param: starting URL
% Param: scanning depth
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter

f_process_main_url(URL, 0, Starts, Contains, Ends) :-
	!,
	% Create results folder
	f_create_dump_folder(Folder),
	% Copy css file to results folder
	generate_css_file(Folder),
	% Write process info
	write('Processing main (0): '),writeln(URL),
	% Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only filtered links
	get_filtered_links(LinkList, [], Starts, Contains, Ends, FilteredLinks),
	% DEBUG: write retrieved data
	%write('Filtered links ->'),writeln(FilteredLinks),nl,
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	write('Generating file: '),writeln(URI),
	f_html_create_document(URI,URL,Starts,Contains,Ends,FilteredLinks,0).

f_process_main_url(URL, N, Starts, Contains, Ends) :-
	% Create results folder
	f_create_dump_folder(Folder),
	% Copy css file to results folder
	generate_css_file(Folder),
	% Write process info
	write('Processing main ('),write(N),write('): '),writeln(URL),
    % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only filtered links
	get_filtered_links(LinkList, [], Starts, Contains, Ends, FilteredLinks),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, [], ValidLinks),
	% Calculate visited links (or being visited)
	append(ValidLinks, [URL], VisitedLinks),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	f_evaluate_level(ValidLinks, M, VisitedLinks, Starts, Contains, Ends, FilteredLinks, NewFiltered),
	% DEBUG: write retrieved data
	%write('Filtered links ->'),writeln(NewFiltered),nl,
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	write('Generating file: '),writeln(URI),
	f_html_create_document(URI,URL,Starts,Contains,Ends,NewFiltered,N),
	!.

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
% Param: starting URL
% Param: scanning depth
% Param: list with the already visited list
% Param: output list with the new visited links
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter
% Param: list with the already filtered links
% Param: output list with the new filtered links

f_process_url(URL, 0, VisitedLinks, VisitedLinks, Starts, Contains, Ends, FLinks, NFLinks) :-
	% Don't try more
	!,
	% Write process info
	write('Processing (0): '),writeln(URL),
	% Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only filtered links
	get_filtered_links(LinkList, FLinks, Starts, Contains, Ends, NFLinks).
	% DEBUG: write retrieved data
	%write('Filtered links ->'),writeln(NFLinks),nl.

% Process and URL with depth > 1. In this case we must build
% the links graph and explore the new websites
f_process_url(URL, N, VisitedLinks, NewVisitedLinks, Starts, Contains, Ends, FLinks, NFLinks) :-
	% Write process info
	write('Processing ('),write(N),write('): '),writeln(URL),
    % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only filtered links
	get_filtered_links(LinkList, FLinks, Starts, Contains, Ends, AuxFLinks),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, VisitedLinks, ValidLinks),
	% Recalculate visited links (althought they haven't been visited yet)
	append(VisitedLinks, ValidLinks, NewVisitedLinks),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	f_evaluate_level(ValidLinks, M, NewVisitedLinks, Starts, Contains, Ends, AuxFLinks, NFLinks),
	% DEBUG: write retrieved data
	%write('Filtered links ->'),writeln(NFLinks),nl,
	!.

% Evaluate remaining levels. We must take care of timeout or redirects
% to HTTPS, so we will take all exceptions and avoid processing the
% associated webs
% Param: list with the remaining links for this level
% Param: scanning deph
% Param: list with the visited links
% Param: links starting filter
% Param: links containing filter
% Param: links ending filter
% Param: list with already filtered links
% Param: output list with the new filtered links

f_evaluate_level([], _ , _, _, _, _, Links, Links).
f_evaluate_level([L|Ls], N, VisitedLinks, Starts, Contains, Ends, FLinks, NFLinks) :-
	catch(
	      % Try section
	      (
	          f_process_url(L, N, VisitedLinks, NewVisitedLinks, Starts, Contains, Ends, FLinks, AuxLinks),
			  append(FLinks, AuxLinks, LevelFLinks),
			  f_evaluate_level(Ls, N, NewVisitedLinks, Starts, Contains, Ends, LevelFLinks, NFLinks)
	      ),
	      % Exception taking
	      _,
	      % Catch section
	      (
		      f_evaluate_level(Ls, N, VisitedLinks, Starts, Contains, Ends, FLinks, NFLinks))
	      ).

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
    	nl,
        writeln('=== Thanks for using Crawler1.0 ==='),
        true
    ;
    	% Set initial time
    	set_i_time,
    	% Process command
    	nl,
        writeln('=== Executing command ==='),
        nl,
        process_command(Codes),
        % Write elapsed time 
        write_f_time,
        % Fail to relaunch crawler prompt
        fail
    ).

% Predicate to start the user input processing
% Param: code list with the user entry

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
% Param: code list to be splitted

split_by_spaces([A|As]) -->
    rm_spaces(_),
    get_chars([X|Xs]),
    {
    	atom_codes(A, [X|Xs])
    },
    rm_spaces(_),
    split_by_spaces(As).
split_by_spaces([]) --> [].

% DCG to consume all valid chars (not spaces) from the entry list
% Param: initial code list

get_chars([X|Xs]) --> 
	get_char(X), !, 
	get_chars(Xs).
get_chars([]) --> [].

% DCG to consume all spaces from the entry list
% Param: initial code list

rm_spaces([X|Xs]) --> 
	get_space(X), !, 
	rm_spaces(Xs).
rm_spaces([]) --> [].

% DCG to check for spaces
% Param: character code to be checked

get_space(X) --> 
	[X], 
	{
		% Check for space code
		code_type(X, space)
	}.

% DCG to check for chars except spaces
% Param: character code to be checked

get_char(X) --> 
	[X], 
	{
		% Check for any code except space one
		\+ code_type(X, space)
	}.

% Check and execute a command
% Param: atom list with the command in the head and all options as the tail

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
    % Check for valid URL
    (startsWithHttp(URL) ->
    	writeln('Valid URL.')
    	;
    	(writeln('Invalid URL. Ensure it starts with HTTP protocol (SSL not supported).'),
       	fail)
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
    	write('Scanning links starting with: '),
       	writeln(Starts)
       	)
       	;
       	(Starts = '',
       	write('Scanning links starting with: '),
       	writeln(Starts))
    ),
    (get_param('-c', FOptions, Contains) ->
       	(
    	write('Scanning links containing: '),
       	writeln(Contains)
       	)
       	;
       	(Contains = '',
       	write('Scanning links containing: '),
       	writeln(Contains))
    ),
    (get_param('-e', FOptions, Ends) ->
       	(
    	write('Scanning links ending with: '),
       	writeln(Ends)
       	)
       	;
       	(Ends = '',
       	write('Scanning links ending with: '),
       	writeln(Ends))
    ),
    write('Execute: scan -u '),write(URL),
    write(' -d '),write(Depth),
    write(' -s '),write(Starts),
    write(' -c '),write(Contains),
    write(' -e '),writeln(Ends),
    % Run command
    process_main_url(URL, Depth, Starts, Contains, Ends),
    nl,
    writeln('== Finished search. All info dumped to a new folder. ==').

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
    % Check for valid URL
    (startsWithHttp(URL) ->
    	writeln('Valid URL.')
    	;
    	(writeln('Invalid URL. Ensure it starts with HTTP protocol (SSL not supported).'),
       	fail)
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
    write(' -e '),writeln(Ends),
    % Run command
    f_process_main_url(URL, Depth, Starts, Contains, Ends),
    nl,
    writeln('== Finished search. All info dumped to a new folder. ==').

check_and_execute([help | _]) :-
	writeln('=== Crawler help ==='),nl,
	writeln('1.- Command to retrieve data from a starting URL'),nl,
	writeln('scan -u URL [-d Depth] [-s Starts] [-c Contains] [-e Ends]'),nl,
	writeln('  1.1.- Parameters:'),
    writeln('    -u : URL used as starting point. It must be a valid web URL. HTTP over SSL is not supported in 1.0 version.'),
    writeln('    -d : (Optional) Scanning detph. It must be a value greater or equal than zero. It is zero by default, which means that only the base URL will be scanned.'),
	writeln('    -s : (Optional) Scan only links which starts with the given pattern.'),
	writeln('    -c : (Optional) Scan only links which contains the given pattern.'),
	writeln('    -e : (Optional) Scan only links which ends with the given pattern.'),nl,
	writeln('  1.2.- Examples:'),	
	writeln('scan -u http://www.fdi.ucm.es -d 2 -> Scans for all data starting at FDI web and with depth 2'),
	nl,
	writeln('2.- Command to retrieve filtered URLs from a starting one'),nl,
	writeln('find -u URL [-d Depth] [-s Starts] [-c Contains] [-e Ends]'),nl,
	writeln('  2.1.- Parameters:'),
    writeln('    -u: URL used as starting point. It must be a valid web URL. HTTP over SSL is not supported in 1.0 version.'),
    writeln('    -d : (Optional) Scanning detph. It must be a value greater or equal than zero. It is zero by default, which means that only the base URL will be scanned.'),
	writeln('    -s : (Optional) Search for links which starts with the given pattern.'),
	writeln('    -c : (Optional) Search for links which contains the given pattern.'),
	writeln('    -e : (Optional) Search for links which ends with the given pattern.'),nl,
	writeln('  2.2.- Examples:'),
	writeln('find -u http://www.fdi.ucm.es -e .pdf -> Finds all linked PDF docs in the FDI website'),
	writeln('find -u http://www.fdi.ucm.es -d 1 -c turing -> Finds all links which contains the "turing" word starting at the FDI website and with depth 2'),nl,
	writeln('3.- Command to exit crawler'),nl,
	writeln('exit'),nl,
	writeln('4.- Additional info'),nl,
	writeln('  4.1.- CPU, RAM and time usage'),
	writeln('Users must take into account that exploring in depths greater than 2 or 3 starting at well known webs may return an enormous amount of results. This may produce:'),
	writeln('  - High RAM usage: this may need user to increase the amount of RAM used by SWI-Prolog to avoid stack overflow problems.'),
	writeln('  Another possible approach is to use the scan aux. params (-c,-s,-e) to limit the scanning process and search/deep only for desired links.'),
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
% Param: input command parameters list
% Param: output formatted options list

format_options([],[]).
format_options([ Type, Value | ROp], FOp) :-
	format_options(ROp, FO1),
	append([(Type,Value)], FO1, FOp),
	!.
format_options :-
	writeln('Invalid options format.  Please check your syntax or type "help" to list all available commands.'),
    fail.

% Predicate to get one needed param from the list
% Param: type of param to be searched (i. e.: '-u')
% Param: list of user params
% Param: output value for that param

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
% Param: type of param to be searched (i. e.: '-c')
% Param: list of user params
% Param: output value for that param

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
% Param: type of param to be searched (i. e.: '-d')
% Param: list of user params
% Param: output value for that param

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

%-------------------%
%  AUX. PREDICATES  %
%-------------------%

% Set the initial time to be compared once the execution finishes

set_i_time:- 
	retractall(i_time(_)), 
	get_time(T),
  	assert(i_time(T)).

% Predicate to compare the initial with the ending time and ouputs
% the elapsed time

write_f_time:-
        get_time(T2), 
        i_time(T1), 
        T is (T2-T1),
        nl,
        writeln('==============='),
        write('Elapsed time: '),write(T), writeln(' secs.'),
        writeln('===============').

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with FDI URL and no exploring depth
test1 :- process_main_url('http://www.fdi.ucm.es/',0,'','','').

% Test predicate to check for links list composing
test2(L,C) :- cleanly_load_html('http://www.mitmiapp.com',DOM),
	get_link_list(DOM, L),
	get_all_style_list(DOM, C).

% Test whether an HTML has some style code
test3 :- cleanly_load_html('http://www.mitmiapp.com',DOM),
	uses_style(DOM).

% Test whether an HTML has some JS code
test4 :- cleanly_load_html('http://www.mitmiapp.com',DOM),
	uses_js(DOM).

% Test predicate with Mitmi URL and exploring depth
test5 :- process_main_url('http://www.mitmiapp.com/',1,'','','').

% Test predicate with Fdi URL and exploring depth
test6 :- process_main_url('http://www.fdi.ucm.es/',1,'','','').

% Test predicate with Fdi URL and optional exploring depth
test7(D) :- process_main_url('http://www.fdi.ucm.es/',D,'','','').

% This predicate fails, it should be debugged!!!
test8 :- process_main_url('http://www.fdi.ucm.es/',2,'','','').

% This predicate fails, it should be debugged!!!
test9 :- process_main_url('http://www.philosophyofinformation.net/unesco/.html',0,'','','').

% Test predicate with Fdi URL and optional exploring depth
test10(D) :- process_main_url('http://www.fdi.ucm.es/',D,'','','').

% General predicate test
genTest(URL, Depth, Starts, Contains, Ends) :- process_main_url(URL, Depth, Starts, Contains, Ends).

% General predicate test
genTestF(URL, Depth, Starts, Contains, Ends) :- f_process_main_url(URL, Depth, Starts, Contains, Ends).

% Initial test for finding crawler
test11(D, Starts, Contains, Ends) :- f_process_main_url('http://www.fdi.ucm.es/',D,Starts,Contains,Ends).

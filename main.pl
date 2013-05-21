:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(url)).
:- use_module(library(http/html_write)).

%-------------------%
% STRING PREDICATES %
%-------------------%

% Test wither an URL ends with the given pattern
endsWith(URL, Pattern) :- sub_string(URL,_,_,0,Pattern).

% Test whether an URL starts with the given pattern
startsWith(URL, Pattern) :- sub_string(URL,0,_,_,Pattern).

% Test whether an URL contains the given pattern
containsPattern(URL, Pattern) :- sub_string(URL, _, _, _, Pattern).

% Test whether an URL starts with 'http://'
startsWithHttp(URL) :- sub_string(URL,0,_,_,'http://').

% Test whether an URL starts with 'https://'
startsWithHttps(URL) :- sub_string(URL,0,_,_,'https://').

% Extract host name from the given URL
extract_host_name(URL,Host) :-
		parse_url(URL, Attr),
		get_host(Attr,Host),!.
extract_host_name(URL,URL).

% This function gets the host name from the URL attributes
% retrieved with URL library
get_host([],_) :- fail.
get_host([host(H) | _],H) :- !.
get_host([_ | Ls], H) :- get_host(Ls, H).

% DCG to replace characters
eos([], []).

replace(_, _) --> call(eos), !.
replace(Find, Replace), Replace -->
        Find,
        !,
        replace(Find, Replace).
replace(Find, Replace), [C] -->
        [C],
        replace(Find, Replace).

substitute(Find, Replace, Request, Result) :-
        phrase(replace(Find, Replace), Request, Result).

%------------------%
% LINKS PREDICATES %
%------------------%

% Test whether an URL is a valid link to be processed. Actually we
% only support HTTP (not SSL) connections.
is_valid_link(URL) :- startsWithHttp(URL),
	% We only support exploring html valid formats
	(
	    is_valid_output(URL);
	    is_valid_host(URL)
	).

% Test if an URL is a valid HTML content output
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
is_valid_host(URL) :-
		extract_host_name(URL, Host),
		endsWith(URL, Host).

% Get all link labels from a DOM structure
get_link_labels(DOM, HREF):-
	% Using XPath expression to retrieve links list
	xpath(DOM,//a(@href),HREF).

% Get all links list
get_all_link_list(DOM, List) :-
	setof(L, xpath(DOM,//a(@href),L), List).

% Get all HTTP or HTTPS links (in list form)
get_link_list(DOM, List) :-
	setof(L,
	      (xpath(DOM,//a(@href),L),
	      (startsWithHttp(L);startsWithHttps(L))),
	      List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_link_list(_,[]).

% Get only valid links
get_valid_links([],_,[]).
get_valid_links([X|Xs], VisitedLinks, [X|Ys]) :-
	\+member(X, VisitedLinks),
	is_valid_link(X),!,
	append(VisitedLinks, [X], NewVisitedLinks),
	get_valid_links(Xs, NewVisitedLinks, Ys).
get_valid_links([_|Xs], VisitedLinks, Ys) :-
	get_valid_links(Xs, VisitedLinks, Ys).

% This predicate tests if the given URL passes all user filters
filter_url(URL, Starts, Contains, Ends) :-
	startsWith(URL, Starts),
	containsPattern(URL, Contains),
	endsWith(URL, Ends).

% Filter retrieved links with the given patterns
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
get_all_style_list(DOM, List) :-
	setof(L, xpath(DOM,//link(@rel='stylesheet',@href),L), List),!.
% Clause needed to avoid style retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_style_list(_,[]).

% This predicate tests if there is some style in the given HTML.
% It is neccesary because a style section may not be linked via <link>
% label
uses_style(DOM) :- xpath(DOM,//style,_);
                   xpath(DOM,//link(@type='text/css'),_).

%---------------%
% JS PREDICATES %
%---------------%

% Get all Javascript links list
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
uses_js(DOM) :- xpath(DOM,//script(@type='text/javascript'),_).

% Predicate to init the JS file for the graph
init_graph_js(Folder, FileName, Stream) :-
	% Compose JS file path
	append(Folder,"/js/",F1),
	append(F1, FileName, F2),
	append(F2, ".js", JsFile),
	name(JsFilePath, JsFile),
	% Open file
	open(JsFilePath, write, Stream),
	% Init file
	write(Stream, '$(document).ready(function() {'), nl(Stream),
	write(Stream, 'var width = $(window).width();'), nl(Stream),
	write(Stream, 'var height = $(window).height();'), nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
init_graph_js(_, _, _) :-
	write('Error: cannot create the JS file. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to generate one graph head
head_graph_js(Stream, Name) :-
	name(Name, N),
	append("var g", N, H1),
	append(H1, " = new Graph();", H2),
	name(GraphHead, H2),
	write(Stream, GraphHead),
	nl(Stream),
	append("g", N, H3),
	append(H3, ".edgeFactory.template.style.directed = true;", H4),
	name(GraphDirected, H4),
	write(Stream, GraphDirected),
	nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
head_graph_js(_, _) :-
	write('Error: cannot create the graph head. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to generate one graph ending
ending_graph_js(Stream, Name) :-
	name(Name, N),
	append("var layouter", N, E1),
	append(E1, " = new Graph.Layout.Spring(g", E2),
	append(E2, N, E3),
	append(E3, ");", E4),
	name(Layouter, E4),
	write(Stream, Layouter),
	nl(Stream),
	append("var renderer", N, E21),
	append(E21, " = new Graph.Renderer.Raphael('canvas", E22),
	append(E22, N, E23),
	append(E23, "', g", E24),
	append(E24, N, E25),
	append(E25, ", width, height);", E26),
	name(Renderer, E26),
	write(Stream, Renderer),
	nl(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
ending_graph_js(_, _) :-
	write('Error: cannot create the graph ending. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to close the JS file for the graph
close_graph_js(Stream) :-
	write(Stream, '});'),
	% Close file
	close(Stream),
	!.
% In case of fail, we cannot continue cause JS is needed for the graph
close_graph_js(_) :-
	write('Error: cannot create the JS file. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to dump the complete graph into the JS file
full_js_graph([], _, _).
full_js_graph([V1-V2|Xs], Stream, Name) :-
		name(Name, N1),
		append("g", N1, N2),
		append(N2, ".addEdge(\"", N3),
		name(GStart, N3),
		write(Stream, GStart),
		write(Stream, V1),
		write(Stream, '" , "'),
		write(Stream, V2),
		write(Stream, '");'),
		nl(Stream),
		!,
		full_js_graph(Xs, Stream, Name).
% Continue dump althought one step fails
full_js_graph([_|Xs], Stream, Name) :-
		full_js_graph(Xs, Stream, Name),!.

% Predicate to dump one graph section into the JS file
partial_js_graph(_, [], _, _).
partial_js_graph(Root, [X|Xs], Stream, Name) :-
		name(Name, N1),
		append("g", N1, N2),
		append(N2, ".addEdge(\"", N3),
		name(GStart, N3),
		write(Stream, GStart),
		write(Stream, Root),
		write(Stream, '" , "'),
		write(Stream, X),
		write(Stream, '");'),
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
get_all_meta_list(DOM, List) :-
	setof(L, xpath(DOM,//meta,L), List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_meta_list(_, []).

% Get HTML5 charset metatag
get_html_charset([],'No defined HTML5 charset tag').
get_html_charset([M|_], Charset) :-
	xpath(M,//meta(@charset),Charset),!.
get_html_charset([_|MetaTags],Charset) :-
	get_html_charset(MetaTags,Charset).

% Get all content metatags. They will be grouped into pairs with the
% form ContentType-ContentValue
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
cleanly_load_html(URL,DOM) :-
	load_html(URL,DOM),!.
cleanly_load_html(_,[]).

% This predicate creates an HTML output document and dumps
% all retrieved data (finding crawler)
f_html_create_document(URI,Title,Starts,Contains,Ends,Links,Depth) :-
	phrase(f_html_structure(Title,Starts,Contains,Ends,Links,Depth), Tokens),
	open(URI, write, Stream),
	print_html(Stream,Tokens),
	close(Stream).

% Predicate to generate the HTML structure to be dumped
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
html_create_document(URI,Title,Charset,Styles,Js,Metas,Graph,CompleteGraph) :-
	phrase(html_structure(Title,Charset,Styles,Js,Metas,Graph,CompleteGraph), Tokens),
	open(URI, write, Stream),
	print_html(Stream,Tokens),
	close(Stream).

% Predicate to generate the HTML structure to be dumped
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

% This predicate creates an HTML output document and dumps
% the hosts graph
html_create_graph_document(URI,Title,Graph,Folder) :-
	phrase(html_graph_structure(Title,Graph,Folder), Tokens),
	open(URI, write, Stream),
	print_html(Stream,Tokens),
	close(Stream).

html_graph_structure(Title,Graph,Folder) -->
		page([title(['Hosts graph']),
			meta(['http-equiv'('content-type'),content('text/html; charset=utf-8')]),
			link([rel('stylesheet'),type('text/css'),href('css/main.css')]),
			script([type('text/javascript'),src('js/raphael-min.js')],''),
			script([type('text/javascript'),src('js/dracula_graffle.js')],''),
			script([type('text/javascript'),src('js/jquery-1.4.2.min.js')],''),
			script([type('text/javascript'),src('js/dracula_graph.js')],''),
			script([type('text/javascript'),src('js/dracula_algorithms.js')],''),
			script([type('text/javascript'),src('js/index.js')],'')
			],
			[
			   \dump_graph_to_html(Title,Graph,Folder)
            ]).

% Predicate to dump the graph HTML content
dump_graph_to_html(Title,Graph,Folder) -->
	{vertices(Graph, V),
	length(V, L),
	% Check if the complete graph is small enought
	L < 20, !},
	dump_one_graph(Title,Graph,Folder).

dump_graph_to_html(_,Graph,Folder) -->
	{
		init_graph_js(Folder, "index", Stream)
	},
	dump_multiple_graphs(Graph,Stream,0),
	{
		close_graph_js(Stream),
		!
	}.

dump_graph_to_html(_,_,_) -->
	{writeln('Error: graph has not been created. Aborting execution.')},
	[].

% Generate one graph JS
dump_one_graph(Title,Graph,Folder) -->
	{
		edges(Graph, Edges),
		init_graph_js(Folder, "index", Stream),
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
			(L > 20) ->
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

% Predicate to create the output HTML index
create_index -->
	    {name(CCharset,"#Charset")},
	    {name(CStyle,"#Style")},
	{name(CJS,"#JS")},
	{name(CMeta,"#Meta")},
	{name(CHTable,"#HostsTable")},
	{name(CLTable,"#LinksTable")},
		html([
		tr([ td(
				a([href('graph.html'),target('_blank')],'Hosts graph')
			)
		]),
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
create_linked_rows(_, []) -->
        [].
create_linked_rows(BaseUrl, [X|Xs]) -->
		% Generate link URL
		{global_url(X, BaseUrl, GLink)},
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

% Predicate for meta tags rows
create_meta_rows([]) -->
        [].
create_meta_rows([T1:C1|Xs]) -->
        html([ tr([ td(T1)
                   ,
		    td(C1)]
		  )
             ]),
        create_meta_rows(Xs).

% Predicate to dump the complete graph in text form
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
not_headed_node([N-[]|_], N).
not_headed_node([_|Xs], N) :-
	not_headed_node(Xs, N).

%---------------------------%
% FILES & FOLDER PREDICATES %
%---------------------------%

% Predicate to copy one file to another. It will be used to
% auto copy the CSS file to every output folder
copy(File1, File2) :-
	open(File1,read,Stream1),
	open(File2,write,Stream2),
	copy_stream_data(Stream1,Stream2),
	close(Stream1),
	close(Stream2),!.
% Clause needed to avoid problems. If CSS is not copied the system
% can continue running without problems.
copy(_, _).

% Predicate to create the ouput folder (scanning crawler)
create_dump_folder(Folder, ContentFolder) :-
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
	make_directory(Folder),
	% Create folder to dump all secondary web data
	append(Folder, "/other_data",ContentFolder),
	make_directory(ContentFolder),
	!.
create_dump_folder(_, _) :-
	write('Error: cannot create the output folder. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to create the ouput folder (finding crawler)
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
	make_directory(Folder),
	!.
f_create_dump_folder(_) :-
	write('Error: cannot create the output folder. '),
	writeln('Please, check you have got the right permissions.'),
	fail.

% Predicate to copy the CSS files to the ouput folder
generate_css_file(Folder) :-
	append(Folder,"/css",CssDirectory),
	make_directory(CssDirectory),
	append(CssDirectory,"/main.css",CssFile),
	name(CssFilePath, CssFile),
	copy('crawler_css/main.css',CssFilePath),!.
generate_css_file(_) :-
	write('Warning: cannot create the css folder. '),
	writeln('Please, check you have got the right permissions.').

% Predicate to copy the JS files to the ouput folder
generate_js_files(Folder) :-
	append(Folder,"/js",JsDirectory),
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
	% Fourth JS file
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
generate_graph(BaseUrl,[],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	% Set graph root (base url)
	add_vertices([],[Host],Graph),!.
generate_graph(BaseUrl,[L|Ls],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	generate_graph(Host,Ls,G1),
	% Add current URL to graph (needed?)
	add_vertices(G1,[Host],G2),
	% Get host name (if possible)
	extract_host_name(L,LHost),
	add_edges(G2,[Host-LHost],Graph).

% Generate a graph in depth with the given params. In this case we
% won't trim to the host name
generate_complete_graph(BaseUrl,[],Graph) :-
	% Set graph root (base url)
	add_vertices([],[BaseUrl],Graph),!.
generate_complete_graph(BaseUrl,[L|Ls],Graph) :-
	generate_complete_graph(BaseUrl,Ls,G1),
	% Add current URL to graph (needed?)
	add_vertices(G1,[BaseUrl],G2),
	% Add edge
	add_edges(G2,[BaseUrl-L],Graph).

%---------------------------%
%  DATA RETRIEVING CRAWLER  %
%---------------------------%

% Predicate to process the base URL. We need this to apply some
% changes to main URL and create the data dump folder
process_main_url(URL, 0, OutGraph, OutCompleteGraph) :-
	!,
	% Create results folder
	create_dump_folder(Folder, _),
	% Copy css file to results folder
	generate_css_file(Folder),
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
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph),
	append(Directory,"graph.html",GraphURI),
	name(GURI,GraphURI),
	html_create_graph_document(GURI,URL,OutGraph,Folder).

process_main_url(URL, N, OutGraph, OutCompleteGraph) :-
	% Create results folder
	create_dump_folder(Folder, ContentFolder),
	% Copy css file to results folder
	generate_css_file(Folder),
	generate_css_file(ContentFolder),
	% Copy js files to results folder
	generate_js_files(Folder),
	% Write process info
	write('Processing main ('),write(N),write('): '),writeln(URL),
    % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, [], ValidLinks),
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
	%write('Css links ->'),writeln(CssLinks),nl,
	%write('Javascript links ->'),writeln(JSLinks),nl,
	%write('Content meta tags ->'),writeln(CMetas),nl,
	%write('Charset ->'),writeln(Charset),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	evaluate_level(ValidLinks, M, Graph, OutGraph, CompleteGraph, OutCompleteGraph, ContentFolder, VisitedLinks),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	%write('Complete graph ->'),writeln(OutCompleteGraph),
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph,OutCompleteGraph),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	%write('Complete graph ->'),writeln(OutCompleteGraph),
	%create_graph_js(OutGraph, Folder, "index"),
	append(Directory,"graph.html",GraphURI),
	name(GURI,GraphURI),
	html_create_graph_document(GURI,URL,OutGraph,Folder),
	!.

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
process_url(URL, 0, OutGraph, OutCompleteGraph, Folder, VisitedLinks, VisitedLinks) :-
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
process_url(URL, N, OutGraph, OutCompleteGraph, Folder, VisitedLinks, NewVisitedLinks) :-
	% Write process info
	write('Processing ('),write(N),write('): '),writeln(URL),
        % Get URL HTML as DOM structure
	cleanly_load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(LinkList, VisitedLinks, ValidLinks),
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
	evaluate_level(ValidLinks, M, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, NewVisitedLinks),
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
evaluate_level([], _ , Graph, Graph, CompleteGraph, CompleteGraph, _, _).
evaluate_level([L|Ls], N, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, VisitedLinks) :-
	catch(
	      % Try section
	      (
	          process_url(L, N, LevelGraph, LevelCompleteGraph, Folder, VisitedLinks, NewVisitedLinks),
	          % Graphs union
	          ugraph_union(Graph, LevelGraph, OGraph1),
	          ugraph_union(CompleteGraph, LevelCompleteGraph, LCGraph1),
			  evaluate_level(Ls, N, OGraph1, OutGraph, LCGraph1, OutCompleteGraph, Folder, NewVisitedLinks)
	      ),
	      % Exception taking
	      _,
	      % Catch section
	      (
		  evaluate_level(Ls, N, Graph, OutGraph, CompleteGraph, OutCompleteGraph, Folder, VisitedLinks))
	      ).

%------------------------%
%  DATA FINDING CRAWLER  %
%------------------------%

% Predicate to process the base URL. We need this to apply some
% changes to main URL and create the data dump folder
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
	f_html_create_document(URI,URL,Starts,Contains,Ends,NewFiltered,N),
	!.

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
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

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with FDI URL and no exploring depth
test1 :- process_main_url('http://www.fdi.ucm.es/',0,_,_).

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
test5 :- process_main_url('http://www.mitmiapp.com/',1,OG,_)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and exploring depth
test6 :- process_main_url('http://www.fdi.ucm.es/',1,OG,_)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and optional exploring depth
test7(D) :- process_main_url('http://www.fdi.ucm.es/',D,OG,_)
	 ,nl,nl,write('OG -> '),writeln(OG).

% This predicate fails, it should be debugged!!!
test8 :- process_main_url('http://www.fdi.ucm.es/',2,OG,CG)
	 ,nl,nl,write('OG -> '),writeln(OG)
	 ,nl,nl,write('CG -> '),writeln(CG).

% This predicate fails, it should be debugged!!!
test9 :- process_main_url('http://www.philosophyofinformation.net/unesco/.html',0,_,_).

% Test predicate with Fdi URL and optional exploring depth
test10(D) :- process_main_url('http://www.fdi.ucm.es/',D,_,_).

% General predicate test
genTest(URL, Depth) :- process_main_url(URL, Depth, _, _).

% General predicate test
genTestF(URL, Depth, Starts, Contains, Ends) :- f_process_main_url(URL, Depth, Starts, Contains, Ends).

% Initial test for finding crawler
test11(D, Starts, Contains, Ends) :- f_process_main_url('http://www.fdi.ucm.es/',D,Starts,Contains,Ends).

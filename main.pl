:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(url)).
:- use_module(library(http/html_write)).

%------------------%
% STRING FUNCTIONS %
%------------------%

% Test wither an URL ends with the given pattern
endsWith(URL, Pattern) :- sub_string(URL,_,_,0,Pattern).

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

%-----------------%
% LINKS FUNCTIONS %
%-----------------%

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
% We will only take into account most commonly used host endings
is_valid_host(URL) :-
	    endsWith(URL,'.com');
	    endsWith(URL,'.edu');
	    endsWith(URL,'.aero');
	    endsWith(URL,'.asia');
	    endsWith(URL,'.biz');
	    endsWith(URL,'.cat');
	    endsWith(URL,'.coop');
	    endsWith(URL,'.info');
	    endsWith(URL,'.int');
	    endsWith(URL,'.jobs');
	    endsWith(URL,'.mobi');
	    endsWith(URL,'.museum')
	    .

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
get_valid_links([],_,_).
get_valid_links([X|Xs], VisitedLinks, [X|Ys]) :-
	\+member(X, VisitedLinks),
	is_valid_link(X),!,
	append(VisitedLinks, [X], NewVisitedLinks),
	get_valid_links(Xs, NewVisitedLinks, Ys).
get_valid_links([_|Xs], VisitedLinks, Ys) :-
	get_valid_links(Xs, VisitedLinks, Ys).
%get_valid_links(DOM, List) :-
%	setof(L, (xpath(DOM,//a(@href),L),is_valid_link(L)), List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
%get_valid_links(_,[]).


%---------------%
% CSS FUNCTIONS %
%---------------%

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

%--------------%
% JS FUNCTIONS %
%--------------%

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

%----------------%
% META FUNCTIONS %
%----------------%

% Get all meta elems in the given HTML (list form)
get_all_meta_list(DOM, List) :-
	setof(L, xpath(DOM,//meta,L), List),!.
% Clause needed to avoid JS retrieving problems. If it is not set,
% sometimes false can be returned and stop execution
get_all_meta_list(_, []).

% Get charset metatag
get_html_charset([],'No defined charset tag').
get_html_charset([M|_], Charset) :-
	xpath(M,//meta(@charset),Charset),!.
get_html_charset([_|MetaTags],Charset) :-
	get_html_charset(MetaTags,Charset).

% This section doesn't work because Prolog doesn't recognize the
% http-equiv attribute. This can be checked later and improved
%get_html_charset([],'No defined charset').
%get_html_charset([M|_], Charset) :-
%	xpath(M,//meta(@charset),Charset),!.
%get_html_charset([M|_], Charset) :-
%	xpath(M,//meta(@http-equiv='content-type'),_),!,
%	xpath(M,//meta(@content),Charset).
%get_html_charset([_|MetaTags],Charset) :-
%	get_html_charset(MetaTags,Charset).

% Get all content metatags. They will be grouped into pairs with the
% form ContentType-ContentValue
get_all_content_meta([], []).
get_all_content_meta([M|MetaTags], [X:Y|CMetas]) :-
	xpath(M,//meta(@name),X),!,
	xpath(M,//meta(@content),Y),
	get_all_content_meta(MetaTags,CMetas).
get_all_content_meta([M|MetaTags], [X:Y|CMetas]) :-
	xpath(M,//meta(@'http-equiv'),X),!,
	xpath(M,//meta(@content),Y),
	get_all_content_meta(MetaTags,CMetas).
get_all_content_meta([_|MetaTags], CMetas) :-
	get_all_content_meta(MetaTags, CMetas).

%----------------%
% HTML FUNCTIONS %
%----------------%

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
	    _,%E,
	 % Catch section. Don't dump exception later. Give an
	 % error info message instead
	    (   % writeln(E) ,
		fail )
	    ).

% This predicate is needed in order to avoid some html reading errors
% and continue executing the crawler
cleanly_load_html(URL,DOM) :-
	load_html(URL,DOM),!.
cleanly_load_html(_,[]).

% This predicate creates an HTML output document and dumps
% all retrieved data
html_create_document(URI,Title,Charset,Styles,Js,Metas,Graph) :-
	phrase(html_structure(Title,Charset,Styles,Js,Metas,Graph), Tokens),
	open(URI, write, Stream),
	print_html(Stream,Tokens),
	close(Stream).

% Predicate to generate the HTML structure to be dumped
html_structure(Title,Charset,Styles,Js,Metas,Graph) -->
		page([title([Title]),
			meta(['http-equiv'('content-type'),content('text/html; charset=utf-8')])
			],
			[ h2(align(center),
                        [Title]
		       ),
	       table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Charset')
                          ]),
		       tr([ td(Charset)
                          ])
                     ]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Style tags')
                          ])
                     |\create_rows(Styles)
                     ]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('JS tags')
                          ])
                     |\create_rows(Js)
                     ]),
               table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th([colspan(2)],
			       'Meta tags')
                          ]),
		       tr([ th([width('50%')],
			       'Type'),
			    th([width('50%')],
			       'Content')
                          ])
                     |\create_meta_rows(Metas)
                     ]),
	        table([ align(center),
                       border(1),
                       width('80%')
                     ],
                     [ tr([ th('Graph')
                          ]),
		       tr([ td(Graph)
                          ])
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

%------------------%
% GRAPHS FUNCTIONS %
%------------------%

% Generate a graph with depth 1 with the given params
generate_graph(BaseUrl,[],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	% Set graph root (base url)
	add_vertices([],[Host],Graph).
generate_graph(BaseUrl,[L|Ls],Graph) :-
	% Get host name (if possible)
	extract_host_name(BaseUrl,Host),
	generate_graph(Host,Ls,G1),
	% Add current URL to graph (needed?)
	add_vertices(G1,[Host],G2),
	% Get host name (if possible)
	extract_host_name(L,LHost),
	add_edges(G2,[Host-LHost],Graph).

%---------------------------%
%  DATA RETRIEVING CRAWLER  %
%---------------------------%

% Predicate to process the base URL. We need this to apply some
% changes to main URL and create the data dump folder
process_main_url(URL, 0, OutGraph) :-
	!,
	% Create results folder
	get_time(TimeStamp),
	name(TimeStamp,Folder),
	make_directory(Folder),
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
	% Generate basic graph
	generate_graph(URL, LinkList, OutGraph),
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
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph).

process_main_url(URL, N, OutGraph) :-
	% Create results folder
	get_time(TimeStamp),
	name(TimeStamp,Folder),
	make_directory(Folder),
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
	% Generate basic graph
	generate_graph(URL, LinkList, Graph),
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
	evaluate_level(ValidLinks, M, Graph, OutGraph, Folder, VisitedLinks),
	% Dump graph
	%write('Graph ->'),writeln(OutGraph),
	% HTML output dumping
    append(Folder,"/",Directory),
	append(Directory,"index.html",DirectoryURI),
	name(URI,DirectoryURI),
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph),
	!.

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
process_url(URL, 0, OutGraph, Folder, VisitedLinks, VisitedLinks) :-
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
	% Generate basic graph
	generate_graph(URL, LinkList, OutGraph),
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
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph).

% Process and URL with depth > 1. In this case we must build
% the links graph and explore the new websites
process_url(URL, N, OutGraph, Folder, VisitedLinks, NewVisitedLinks) :-
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
	evaluate_level(ValidLinks, M, Graph, OutGraph, Folder, NewVisitedLinks),
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
	html_create_document(URI,URL,Charset,CssLinks,JSLinks,CMetas,OutGraph),
	!.

% Evaluate remaining levels. We must take care of timeout or redirects
% to HTTPS, so we will take all exceptions and avoid processing the
% associated webs
evaluate_level([], _ , Graph, Graph, _, _).
evaluate_level([L|Ls], N, Graph, OutGraph, Folder, VisitedLinks) :-
	catch(
	      % Try section
	      (
	          process_url(L, N, LevelGraph, Folder, VisitedLinks, NewVisitedLinks),
	          % Graph union
	          ugraph_union(Graph, LevelGraph, OGraph1),
		  	  evaluate_level(Ls, N, OGraph1, OutGraph, Folder, NewVisitedLinks)
	      ),
	      % Exception taking
	      _,
	      % Catch section
	      (	  
	      	  evaluate_level(Ls, N, Graph, OutGraph, Folder, VisitedLinks))
	      ).

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with FDI URL and no exploring depth
test1 :- process_main_url('http://www.fdi.ucm.es/',0,_).

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
test5 :- process_main_url('http://www.mitmiapp.com/',1,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and exploring depth
test6 :- process_main_url('http://www.fdi.ucm.es/',1,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and optional exploring depth
test7(D) :- process_main_url('http://www.fdi.ucm.es/',D,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% This predicate fails, it should be debugged!!!
test8 :- process_main_url('http://www.fdi.ucm.es/',2,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% This predicate fails, it should be debugged!!!
test9 :- process_main_url('http://www.philosophyofinformation.net/unesco/.html',0,_).

% General predicate test
genTest(URL, Depth) :- process_main_url(URL, Depth, _).

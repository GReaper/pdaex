:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

%-----------------%
% LINKS FUNCTIONS %
%-----------------%

% Test wither an URL ends with the given pattern
endsWith(URL, Pattern) :- sub_string(URL,_,_,0,Pattern).

% Test whether an URL starts with 'http://'
startsWithHttp(URL) :- sub_string(URL,0,_,_,'http://').

% Test whether an URL starts with 'https://'
startsWithHttps(URL) :- sub_string(URL,0,_,_,'https://').

% Test whether an URL is a valid link to be processed. Actually we
% only support HTTP (not SSL) connections.
is_valid_link(URL) :- startsWithHttp(URL),
	% We only support exploring html valid formats
	(
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
	    endsWith(URL,'.xml')
	).

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
get_link_list(_,[]).

% Get only valid links
get_valid_links(DOM, List) :-
	setof(L, (xpath(DOM,//a(@href),L),is_valid_link(L)), List),!.
get_valid_links(_,[]).


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
get_all_content_meta([M|MetaTags], [X-Y|CMetas]) :-
	xpath(M,//meta(@name),X),!,
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
	setup_call_cleanup(http_open(URL, In,
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
					close(In)).

%------------------%
% GRAPHS FUNCTIONS %
%------------------%

% Generate a graph with depth 1 with the given params
generate_graph(BaseUrl,[],Graph) :-
	% Set graph root (base url)
	add_vertices([],[BaseUrl],Graph).
generate_graph(BaseUrl,[L|Ls],Graph) :-
	generate_graph(BaseUrl,Ls,G1),
	% Set graph root (base url)
	add_vertices(G1,[BaseUrl],G2),
	add_edges(G2,[BaseUrl-L],Graph).

%----------------%
%  MAIN PROGRAM  %
%----------------%

% Process URL with no depth (only base URL)
% In this case we only take all HTML info without
% making the depth graph (only basic one)
process_url(URL, 0, OutGraph) :-
	% Get URL HTML as DOM structure
	load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(DOM, ValidLinks),
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
	write('All links ->'),writeln(LinkList),nl,
	write('Valid links ->'),writeln(ValidLinks),nl,
	write('Css links ->'),writeln(CssLinks),nl,
	write('Javascript links ->'),writeln(JSLinks),nl,
	write('Content meta tags ->'),writeln(CMetas),nl,
	write('Charset ->'),writeln(Charset),
	% Dump graph
	write('Graph ->'),writeln(OutGraph),
	% Don't try any predicate more
	!.

% Process and URL with depth > 1. In this case we must build
% the links graph and explore the new websites
process_url(URL, N, OutGraph) :-
        % Get URL HTML as DOM structure
	load_html(URL, DOM),
	% Get all link labels from DOM (list form)
	get_link_list(DOM, LinkList),
	% Get only valid links to process (HTTP)
	get_valid_links(DOM, ValidLinks),
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
	write('All links ->'),writeln(LinkList),nl,
	write('Valid links ->'),writeln(ValidLinks),nl,
	write('Css links ->'),writeln(CssLinks),nl,
	write('Javascript links ->'),writeln(JSLinks),nl,
	write('Content meta tags ->'),writeln(CMetas),nl,
	write('Charset ->'),writeln(Charset),
	% Reduce exploring depth
	M is N-1,
	% Evaluate other levels
	evaluate_level(ValidLinks, M, Graph, OutGraph),
	% Dump graph
	write('Graph ->'),writeln(OutGraph),
	% Don't try any predicate more
	!.

% Evaluate remaining levels. We must take care of timeout or redirects
% to HTTPS, so we will take all exceptions and avoid processing the
% associated webs
evaluate_level([], _ , Graph, Graph).
evaluate_level([L|Ls], N, Graph, OutGraph) :-
	catch(
	      % Try section
	      (
	          process_url(L, N, LevelGraph),
	          % Graph union
	          ugraph_union(Graph, LevelGraph, OGraph1),
		  evaluate_level(Ls, N, OGraph1, OutGraph)
	      ),
	      % Exception taking
	      _,
	      % Catch section
	      (	  evaluate_level(Ls, N, Graph, OutGraph))
	      ).

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with FDI URL and no exploring depth
test1 :- process_url('http://www.fdi.ucm.es/',0,_).

% Test predicate to check for links list composing
test2(L,C) :- load_html('http://www.mitmiapp.com',DOM),
	get_link_list(DOM, L),
	get_all_style_list(DOM, C).

% Test whether an HTML has some style code
test3 :- load_html('http://www.mitmiapp.com',DOM),
	uses_style(DOM).

% Test whether an HTML has some JS code
test4 :- load_html('http://www.mitmiapp.com',DOM),
	uses_js(DOM).

% Test predicate with Mitmi URL and exploring depth
test5 :- process_url('http://www.mitmiapp.com/',1,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and exploring depth
test6 :- process_url('http://www.fdi.ucm.es/',1,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% Test predicate with Fdi URL and optional exploring depth
test7(D) :- process_url('http://www.fdi.ucm.es/',D,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% This predicate fails, it should be debugged!!!
test8 :- process_url('http://www.fdi.ucm.es/',2,OG)
	 ,nl,nl,write('OG -> '),writeln(OG).

% General predicate test
genTest(URL, Depth) :- process_url(URL, Depth, _).






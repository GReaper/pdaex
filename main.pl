:- use_module(library(http/http_open)).
:- use_module(library(xpath)).

%-----------------%
% LINKS FUNCTIONS %
%-----------------%

% Test whether an URL starts with 'http://'
startsWithHttp(URL) :- sub_string(URL,0,_,_,'http://').

% Test whether an URL starts with 'https://'
startsWithHttps(URL) :- sub_string(URL,0,_,_,'https://').

% Test whether an URL is a valid link to be processed. Actually we
% only support HTTP (not SSL) connections.
is_valid_link(URL) :- startsWithHttp(URL).

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
	      List).

% Get only valid links
get_valid_links(DOM, List) :-
	setof(L, (xpath(DOM,//a(@href),L),is_valid_link(L)), List).


%---------------%
% CSS FUNCTIONS %
%---------------%

% Get all stylesheet links list
get_all_style_list(DOM, List) :-
	setof(L, (xpath(DOM,//link(@rel='stylesheet'),L1),
		    xpath(L1,//link(@href),L)), List).


%--------------%
% JS FUNCTIONS %
%--------------%

% Get all Javascript links list
get_all_js_list(DOM, List) :-
	setof(L, (xpath(DOM,//script(@type='text/javascript'),L1),
		    xpath(L1,//script(@src),L)), List).

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
						dialect(sgml),
						shorttag(false),
						max_errors(-1),
						syntax_errors(quiet)
					])
					),
					close(In)).

%----------------%
%  MAIN PROGRAM  %
%----------------%

% Main "entry point".
process_url(URL) :-
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
	% DEBUG: write retrieved links
	write('All links ->'),writeln(LinkList),
	write('Valid links ->'),writeln(ValidLinks),
	write('Css links ->'),writeln(CssLinks),
	write('Javascript links ->'),writeln(JSLinks).

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with Mitmi URL
test1 :- process_url('http://www.mitmiapp.com').

% Test predicate to check for links list composing
test2(L,C) :- load_html('http://www.mitmiapp.com',DOM),
	get_link_list(DOM, L),
	get_all_style_list(DOM, C).


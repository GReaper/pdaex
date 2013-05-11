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
	setof(L, xpath(DOM,//link(@rel='stylesheet',@href),L), List).

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
	      List).

% This predicate tests if there is some JS script in the given DOM.
% It is neccesary because a JS script may not be linked via <script>
% label
uses_js(DOM) :- xpath(DOM,//script(@type='text/javascript'),_).

%----------------%
% META FUNCTIONS %
%----------------%

% Get all meta elems in the given HTML (list form)
get_all_meta_list(DOM, List) :-
	setof(L, xpath(DOM,//meta,L), List).

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
	% Get all meta elems
	get_all_meta_list(DOM, MetaElms),
	% Get HTML charset
	get_html_charset(MetaElms, Charset),
	% Get content metas
	get_all_content_meta(MetaElms, CMetas),
	% DEBUG: write retrieved data
	write('All links ->'),writeln(LinkList),nl,
	write('Valid links ->'),writeln(ValidLinks),nl,
	write('Css links ->'),writeln(CssLinks),nl,
	write('Javascript links ->'),writeln(JSLinks),nl,
	write('Content meta tags ->'),writeln(CMetas),nl,
	write('Charset ->'),writeln(Charset).

%----------------------%
%  TESTING PREDICATES  %
%----------------------%

% Test predicate with Mitmi URL
test1 :- process_url('http://www.fdi.ucm.es/').

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









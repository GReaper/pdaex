:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

prueba1(URL):-
	http_open(URL,In,[]),
	copy_stream_data(In,user_output),
	close(In).
	
prueba2(URL,LI):-
	http_get(URL,In,[]),
	http_read_data(In,D,[]),
	xpath(D,//li, LI),
	close(In).	

get_href(URL,HREF):-
			http_load_html(URL,DOM),
			xpath(DOM,//a(@href),HREF).
			
% http_load_html('http://www.fdi.ucm.es',DOM),xpath(DOM,//a(@href),HREF).	
http_load_html(URL, DOM) :-
        setup_call_cleanup(http_open(URL, In,
                           [ timeout(60)
                           ]),
                           (   dtd(html, DTD),
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
% Test whether  an URL starts with 'http://'
startsWithHttp(T) :- sub_string(T,0,_,_,'http://').

% Test whether an URL starts with 'http://' or 'https://'
isValidLink(T) :- sub_string(T,0,_,_,'http://') ;
                  sub_string(T,0,_,_,'https://').

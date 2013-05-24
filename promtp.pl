%Prueba con la funcion para el prompt
crawler(Prompt):-
	read_history('history', 'help', [trace],Prompt, Goal,_).
	parser_crawler(Goal).

parser_crawler(Goal) -->
		hecho(Command),
		{
		    Command=='search',test1
		}.

expr(E) -->
        term(T),
        contExpr(Op,T2),
        {
         Op=='+',!,E is T+T2
        ;
         Op=='-',!,E is T-T2
        ;
         E=T}.

contExpr('+',E) --> "+", !, expr(E).
contExpr('-',E) --> "-", !, expr(E).
contExpr(_,_) --> "".




term(T) -->
        fact(F), contTerm(Op,F2),
        {
         Op=='*',!,T is F*F2
        ;
         Op=='/',!,T is F/F2
        ;
         T=F}.

contTerm('*',T) --> "*", !, term(T).
contTerm('/',T) --> "/", !, term(T).
contTerm(_,_) --> "".


fact(F) --> "(", !, expr(F), ")".
fact(F) --> entero(F).

entero(E) --> signo(S), enteroSinSigno(Es),
        {
         toDec(Es,0,V),
         (
          S=='-',!,E= -V
         ;
          E=V
         )}.


toDec([],Ac,Ac).
toDec([X|Xs],Ac,V):- Ac1 is Ac*10+X, toDec(Xs,Ac1,V).


enteroSinSigno([D|E]) --> digito(D), contEnteroSinSigno(E).

contEnteroSinSigno([D|E]) --> digito(D), !, contEnteroSinSigno(E).
contEnteroSinSigno([]) --> "".


signo('+') --> "+", !.
signo('-') --> "-", !.
signo(_)   --> "".

digito(D) --> [C], {(C>=48, C=<57),!,D is C-48}.



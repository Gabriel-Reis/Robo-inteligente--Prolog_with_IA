agente :-
   inicia,
   sala(2,X4,Y4),                                                                   %AGENTE
   sala(3,X1,Y1),                                                                   %BLOCO1
   sala(14,X2,Y2),                                                                  %BLOCO2
   sala(23,X3,Y3),                                                                  %BLOCO3
   new(D,dialog('TP01 de IA - Gabriel A., Larissa, Petrus')),
   send(D,append,bitmap(image('FundoNR.jpg'))),
   send(D,display,new(@o1,box(15,15))),
   send(@o1,fill_pattern,colour(red)),
   send(@o1,move,point(X1-23,Y1-15)),                 
   send(D,display,new(@o2,box(15,15))),
   send(@o2,fill_pattern,colour(yellow)),
   send(@o2,move,point(X2-23,Y2)),                             
   send(D,display,new(@o3,box(15,15))),
   send(@o3,fill_pattern,colour(green)),
   send(@o3,move,point(X3-23,Y3+30)),                             
   send(D,append,new(@a,bitmap(image('Agente.jpg')))),
   send(@a,move,point(X4,Y4-20)),
   new(@t,timer(1)),
   send(@t,start),
   send(D,open).

:- dynamic pos/2, seg/1.

inicia :-
   forall(member(X,[@t,@a,@o1,@o2,@o3]),free(X)),
   retractall(pos(_,_)),
   retractall(seg(_)),
   assert(pos(agente,2)),                                                             %POSICAO DO AGENTE
   assert(pos(1,1)),
   assert(pos(2,1)),
   assert(pos(3,1)).

   inicial([2,3,14,23,30,0,0,0,0]).                                                   %AGENTE,B1,B2,B3,DEPOSITO,
   meta([_,3,14,23,30,0,1,1,1]).                                                      %META


% ------------ movimentos

   %ACOES DE MOVIMENTO
   acao(entrar0102,[1,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0201,[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[1,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0203,[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0302,[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0304,[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[4,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0403,[4,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0506,[5,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[6,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0605,[6,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[5,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0708,[7,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[8,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0807,[8,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[7,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0206,[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[6,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar0602,[6,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[2,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar0307,[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[7,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar0703,[7,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[3,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar0509,[5,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).
   acao(entrar0905,[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[5,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).

   acao(entrar0910,[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[10,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1009,[10,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1112,[11,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[12,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1211,[12,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[11,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1415,[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1514,[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1516,[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[16,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1615,[16,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar0913,[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[13,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1309,[13,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[9,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1014,[10,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1410,[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[10,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1115,[11,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1511,[15,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[11,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1216,[12,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[16,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1612,[16,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[12,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1418,[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).
   acao(entrar1814,[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[14,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).

   acao(entrar1718,[17,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1817,[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[17,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1819,[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1918,[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1920,[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[20,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2019,[20,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2324,[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[24,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2423,[24,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar1721,[17,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[21,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2117,[21,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[17,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1822,[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[22,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2218,[22,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[18,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar1923,[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2319,[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[19,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2024,[20,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[24,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2420,[24,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[20,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2327,[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).
   acao(entrar2723,[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[23,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],3).

   acao(entrar2526,[25,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2625,[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[25,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2627,[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2726,[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2728,[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[28,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2827,[28,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar3031,[30,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar3130,[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[30,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar3132,[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[32,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar3231,[32,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],2).
   acao(entrar2529,[25,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[29,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2925,[29,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[25,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2630,[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[30,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar3026,[30,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[26,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2731,[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar3127,[31,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[27,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar2832,[28,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[32,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).
   acao(entrar3228,[32,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],[28,B1,B2,B3,DEPOSITO,ROBO,GUARDA1,GUARDA2,GUARDA3],1).


   acao(pegar1,[B1,B1,B2,B3,DEPOSITO,0,GUARDA1,GUARDA2,GUARDA3],[B1,B1,B2,B3,DEPOSITO,1,GUARDA1,GUARDA2,GUARDA3],0).
   acao(pegar2,[B2,B1,B2,B3,DEPOSITO,0,GUARDA1,GUARDA2,GUARDA3],[B2,B1,B2,B3,DEPOSITO,2,GUARDA1,GUARDA2,GUARDA3],0).
   acao(pegar3,[B3,B1,B2,B3,DEPOSITO,0,GUARDA1,GUARDA2,GUARDA3],[B3,B1,B2,B3,DEPOSITO,3,GUARDA1,GUARDA2,GUARDA3],0).

   acao(soltar1,[DEPOSITO,B1,B2,B3,DEPOSITO,1,0,GUARDA2,GUARDA3],[DEPOSITO,B1,B2,B3,DEPOSITO,0,1,GUARDA2,GUARDA3],0).
   acao(soltar2,[DEPOSITO,B1,B2,B3,DEPOSITO,2,GUARDA1,0,GUARDA3],[DEPOSITO,B1,B2,B3,DEPOSITO,0,GUARDA1,1,GUARDA3],0).
   acao(soltar3,[DEPOSITO,B1,B2,B3,DEPOSITO,3,GUARDA1,GUARDA2,0],[DEPOSITO,B1,B2,B3,DEPOSITO,0,GUARDA1,GUARDA2,1],0).

passagem(X,Y) :- acao(_,[X,_,_,_,_,_,_,_,_],[Y,_,_,_,_,_,_,_,_],_).
passagem(X,Y) :- acao(_,[Y,_,_,_,_,_,_,_,_],[X,_,_,_,_,_,_,_,_],_).

rota(X,X,[X]) :- !.
rota(X,Y,[X|R]) :- passagem(X,Z), rota(Z,Y,R).

movimentar([NOME|P]) :- length(P,X), X = 0 -> movimentar2(NOME), !.
movimentar([NOME|P]) :- 
   acao(NOME,[_,_,_,_,_,_,_,_,_],[X,_,_,_,_,_,_,_,_],_),
   ande(X),
   movimentar2(NOME),
   movimentar(P).

movimentar2(NOME)   :-    NOME = 'pegar1' -> pegue(1).
movimentar2(NOME)   :-    NOME = 'pegar2' -> pegue(2).
movimentar2(NOME)   :-    NOME = 'pegar3' -> pegue(3).
movimentar2(NOME)   :-    NOME = 'soltar1' -> solte(1).
movimentar2(NOME)   :-    NOME = 'soltar2' -> solte(2).
movimentar2(NOME)   :-    NOME = 'soltar3' -> solte(3); !.


% ------------ funcao heuristica: 


 h(_,0).

/*--------------------------------------------------------------+
| Algoritmo de busca no espaco de estados                       |  

| Para executar, descreva o problema de busca e digite:         |
| ?- busca(TipoDeBusca). <enter>                                |
+--------------------------------------------------------------*/  


% selecione o tipo de busca desejada
% 1- aleatoria
% 2- largura
% 3- profundidade
% 4- menor custo
% 5- melhor estimativa
% 6- otima (ou A*)

busca(T) :-
   agente,
   inicial(E),
   busca(T,[_:_:0:E:[]],[],P:G),
   tipo(T,N),
   format('~nTipo.: ~w',[N]),
   format('~nPlano: ~w',[P]),
   format('~nCusto: ~w~n~n',[G]),
   movimentar(P).
busca(_,[_:_:G:E:C|_],_,P:G) :- 
   meta(E), !,
   reverse(C,P).
busca(T,[_:_:G:E:C|F],V,P) :- 
   sucessores(T,G:E:C,V,S),
   insere(T,S,F,NF),
   union([E],V,NV),
   busca(T,NF,NV,P).

sucessores(T,G1:E:C,V,R) :-
   findall(F:H:G:S:[A|C],
      (acao(A,E,S,G2), 
       not(member(S,V)),
       h(S,H), G is G1+G2, 
       (T=4 -> F is G
       ;T=5 -> F is H
       ;T=6 -> F is G+H
       ;       F is 0)),R).
insere(1,S,F,NF) :- append(S,F,R), 
                    length(R,L), embaralha(L,R,NF), !.
insere(2,S,F,NF) :- append(F,S,NF), !.
insere(3,S,F,NF) :- append(S,F,NF), !.
insere(_,S,F,NF) :- append(S,F,R), sort(R,NF), !.

embaralha(0,F,F) :- !.
embaralha(L,F,[X|NF]) :- 
   N is random(L), 
   nth0(N,F,X), delete(F,X,R),
   M is L-1,
   embaralha(M,R,NF), !.

tipo(1,aleatoria).
tipo(2,largura).
tipo(3,profundidade).
tipo(4,menor_custo).
tipo(5,melhor_estimativa).
tipo(6,otima).

% comandos para o agente

ande(L) :- 
   pos(agente,L), !.
ande(L) :- 
   retract(pos(agente,P)), 
   assert(pos(agente,L)), 
   length(R,_), 
   rota(P,L,R), 
   siga(R), !.

pegue(O) :- 
   seg(O), !.
pegue(O) :-
   pos(O,_), 
   %ande(P),         
   retract(pos(O,_)), 
   assert(seg(O)),
   get(@a,position,X),
   obj(O,No,_),
   send(No,move,X), !. 

solte(O) :- 
   not(seg(O)), !.
solte(O) :- 
   pos(agente,P), 
   not(member(P,[5,6])),    
   retract(seg(O)), 
   assert(pos(O,P)), 
   get(@a,position,point(X,Y)),
   obj(O,No,Yo), X1 is X-15, Y1 is Y+Yo,
   send(No,move,point(X1,Y1)), !. 

siga([]).
siga([S|R]) :- mova(S), send(@t,delay), siga(R).

mova(S) :- 
   sala(S,X,Y), 
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
   send(@a,move,point(X,Y)).


sala(1,   43,  46).
sala(2,  131,  46).
sala(3,  219,  46).
sala(4,  307,  46).
sala(5,   43, 130).
sala(6,  131, 130).
sala(7,  219, 130).
sala(8,  307, 130).
sala(9,   43, 214).
sala(10, 131, 214).
sala(11, 219, 214).
sala(12, 307, 214).
sala(13,  43, 299).
sala(14, 131, 299).
sala(15, 219, 299).
sala(16, 307, 299).
sala(17,  43, 382).
sala(18, 131, 382).
sala(19, 219, 382).
sala(20, 307, 382).
sala(21,  43, 460).
sala(22, 131, 460).
sala(23, 219, 460).
sala(24, 307, 460).
sala(25,  43, 556).
sala(26, 131, 556).
sala(27, 219, 556).
sala(28, 307, 556).
sala(29,  43, 641).
sala(30, 131, 641).
sala(31, 219, 641).
sala(32, 307, 641).

obj(1, @o1, -20).
obj(2, @o2,   0).
obj(3, @o3, +20).
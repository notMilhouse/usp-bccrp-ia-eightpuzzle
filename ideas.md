- definir como conhecimento as distancias do estado ao estado final

123
804
765

123
084
765
1

231
084
765

2- 1
3- 1
1- 2
8- 1
h - 5

posA(1,0)
posA(2,1)
posA(3,2)

funcao_heuristica(estadoAtual, estadoFinal, H + G)
    manhattan(estadoAtual, H)
    deslocado(estadoAtual, G)

pra cada elemento
    [a,b,c,d,e,f,g,h,i]

desloc(0, 0).
desloc(X, 1) :-
    X \= 0.
    

manhattan([a,b,c,d,e,f,g,h,i], H, G)
   posA(a, X),
   desloc(X, XDesloc)
   posB
   posC
    ...,
   H = X + Y + ....



1 2 3
4 5 6
7 8 _

> > >
> > >
> > >


1 2 3
8 0 4
7 6 5

> > >
> _ v
^ < <

5 4 0
6 1 8
7 3 2

-> 16

8 1 3
7 _ 2
6 5 4

8 1 3
7 2 _
6 5 4
subtermHelper(T,[],T):- T = node(E,E1,E2).
subtermHelper(node(E,E1,E2),[Head|Tail],T1):- Head =1 , subtermHelper(E1,Tail,T1).
subtermHelper(node(E,E1,E2),[Head|Tail],T1):- Head =2 , subtermHelper(E2,Tail,T1).

subterm(T2,P,T1):-subtermHelper(T2,P,T1).
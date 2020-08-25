lookupTable([],V,T):-false.
lookupTable([Head|Tail],V,T):- Head = (V,T); lookupTable(Tail,V,T).

hastype(Gamma,true,boolT).
hastype(Gamma,false,boolT).
hastype(Gamma,E,intT):-number(E).
hastype(Gamma,E,T):-lookupTable(Gamma,E,T),string(E).

hastype(Gamma,neg(E),intT):-hastype(Gamma,E,intT).
hastype(Gamma,add(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,subt(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,mult(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,div(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,exp(E1,E2),intT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

hastype(Gamma,and(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,or(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,not(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).

hastype(Gamma,equal(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,lessThan(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,greaterThan(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,lessThanEqual(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,greaterThanEqual(E1,E2),boolT):-hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

hastype(Gamma,equal(E1,E2),boolT):-hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).

hastype(Gamma,if_then_else(B,E1,E2),T):-hastype(Gamma,B,boolT),hastype(Gamma,E1,T),hastype(Gamma,E2,T).

hastype(Gamma,def(V,E),T):-hastype(Gamma,E,T),string(V).


typeElaboratesHelper2([],(V,T),[(V,T)]).
typeElaboratesHelper2([(V1,T1)|Tail],(V,T),[Head|Tail1]):- (V1=V -> Head=(V,T);Head=(V1,T1)),(V1=V->Tail1=Tail;typeElaboratesHelper2(Tail,(V,T),Tail1)).


typeElaboratesHelper(Gamma,[],Gamma).
typeElaboratesHelper(Gamma,[Head|Tail],Gamma2):-typeElaboratesHelper2(Gamma,Head,Gamma1),typeElaboratesHelper(Gamma1,Tail,Gamma2).

typeElaborates(Gamma,def(V,E),Gamma2):-hastype(Gamma,E,T),Gamma2=[(V,T)].


typeElaborates(Gamma,seqdef(D1,D2),Gamma2):-typeElaborates(Gamma,D1,[(V,T)]),typeElaboratesHelper(Gamma,[(V,T)],Gamma1),typeElaborates(Gamma1,D2,Gamma3),typeElaboratesHelper([(V,T)],Gamma3,Gamma2).

typeElaborates(Gamma,paradef(D1,D2),Gamma2):-typeElaborates(Gamma,D1,[(V,T)]),typeElaborates(Gamma,D2,Gamma1),typeElaboratesHelper([(V,T)],Gamma1,Gamma2).

typeElaborates(Gamma,localdef(D1,D2),Gamma2):-typeElaborates(Gamma,D1,[(V,T)]),typeElaboratesHelper(Gamma,[(V,T)],Gamma1),typeElaborates(Gamma1,D2,Gamma2).


hastype(Gamma,let_in_end(D,E),T):-typeElaborates(Gamma,D,Gamma1),typeElaboratesHelper(Gamma,Gamma1,Gamma2),hastype(Gamma2,E,T).

hastype(Gamma,nTuple([]),cartesian([])).
hastype(Gamma,nTuple([E|Tail]),cartesian([T|Tail2])):-hastype(Gamma,E,T),hastype(Gamma,nTuple(Tail),cartesian(Tail2)).

typeElaborates(Gamma,par([]),Gamma).
typeElaborates(Gamma,par([Head|Tail]),Gamma2):-hastype(Gamma,Head,T),typeElaboratesHelper(Gamma,[(Head,T)],Gamma1),typeElaborates(Gamma1,par(Tail),Gamma2).
hastype(Gamma,abstract(par(L),E),T):-hastype(Gamma,nTuple(L),T1),typeElaborates(Gamma,par(L),Gamma2),hastype(Gamma2,E,T2),T=arrow(T1,T2).


hastype(Gamma,app(F,E),T):-hastype(Gamma,F,arrow(T1,T)),hastype(Gamma,E,T1).


projHelper(Gamma,[Head|Tail],0,T):-hastype(Gamma,Head,T).
projHelper(Gamma,[Head|Tail],E,T):-F is E-1,projHelper(Gamma,Tail,F,T).
hastype(Gamma,proj(L,E),T):-projHelper(Gamma,L,E,T),number(E).
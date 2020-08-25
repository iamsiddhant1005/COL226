father(vin,aby).
father(ral,nic).
father(chris,vin).
father(vin,sky).
mother(gina,vin).
mother(nic,aby).
mother(ala,nic).
mother(nic,sky).
male(aby).
female(nic).
female(ala).
female(gina).
female(sky).
male(ral).
male(vin).male(chris).
son(X,Y):-father(Y,X),male(X).
grandfather(X,Y):-father(X,K),father(K,Y).
grandson(X,Y):-son(X,K),son(K,Y).
married(X,Y):-father(X,K),mother(Y,K).

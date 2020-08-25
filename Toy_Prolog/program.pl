edge(one,two).
edge(two,three).
edge(four,three).
edge(five,three).
path(X,Y):-edge(X,Z),edge(Y,Z).
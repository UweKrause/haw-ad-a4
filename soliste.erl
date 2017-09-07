-module(soliste).

-author("Sean Pedersen").
-author("Uwe Krause").

-compile(export_all).

% create: ∅ → list
create() -> [].

% isEmpty: list → bool
isEmpty(L) -> L == create().

% isList: list → bool
isList([_H|T]) -> isList(T);
isList([]) -> true;
isList(_) -> false.

%equal: list × list → bool / equal(<Liste>,<Liste>)
equal([E|T1], [E|T2]) -> equal(T1, T2);
equal([], []) -> true;
equal(_, _) -> false.

%laenge: list → int / laenge(<Liste>)
laenge([]) -> 0;
laenge([_H|T]) -> 1 + laenge(T).

% insert:  list × elem → list
insert(L, E) -> [E|L].

% delete: list × elem → list
delete([E|T], E) -> T;
delete([H|T], E) -> [H|delete(T, E)];
delete([], _E) -> [].

% finds: list × elem → pos
finds([E|_T], E) -> 1;
finds([_H|T], E) -> N = finds(T, E),
                    case N > 0 of
                        false -> 0;
                        true -> N + 1
                    end;
finds([],_E) -> 0.

% findmf: list × elem → {pos,list}
findmf(L, E) -> DList = delete(L, E),
                case DList == L of
                    true -> {0, L};
                    false -> {1, [E|DList]}
                end.


                
% findtp: list × elem → {pos,list}
findtp(L=[E|_T], E) -> {1, L};

findtp(L, E) -> findtp_(L, E, 1, []).

% LOOK AHEAD von 1
% Element gefunden
findtp_([H1,E|T], E, Counter, Rest) -> {Counter, Rest++[E]++[H1]++T};

% Element nicht gefunden -> suche weiter in H2+Tail
findtp_([H1,H2|T], E, Counter, Rest) -> findtp_([H2|T], E, Counter+1, Rest++[H1]);

% Fehlerfall - Element nicht in Liste
findtp_([E], _, _, Rest) -> {0, Rest++[E]}.


% retrieve: list × pos → elem
retrieve(L, Pos) -> retrieve_(L, Pos, 1).
retrieve_([H|_T], Pos, Pos) -> H;
retrieve_([_H|T], Pos, Counter) -> retrieve_(T, Pos, Counter+1);
retrieve_([], _Pos, _Counter) -> nil. % nil = NOT IN LIST











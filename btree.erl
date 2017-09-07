% Authors: Uwe Krause, Sean Pedersen
-module(btree).

-export([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2, testALL/0]).


% leerer Baum wird erstellt
initBT() -> {}.


isBT(B) -> {_LMin, _RMax, _H, Gueltigkeit} = reku(B), (Gueltigkeit or isEmptyBT(B)).

% Rueckgabe-Tupel: {Lmin, Rmax, Hoehe, Gueltigkeit}

% Blatt
reku({W, {}, {}, H}) -> {W, W, H, is_number(W) and is_number(H) and (H == 1)};

% Links weiter
reku({W, L, {}, H}) ->  {ULMin, _URMax, UH, UG} = reku(L),
                        {ULMin, W, H, (UG and is_number(W) and is_number(H) and (W > ULMin) and (UH + 1 == H))};


% Rechts weiter
reku({W, {}, R, H}) ->  {_ULMin, URMax, UH, UG} = reku(R),
                        {W, URMax, H, (UG and is_number(W) and is_number(H) and (W =< URMax) and (UH + 1 == H))};

% Beide weiter
reku({W, L, R, H}) ->   {UL_LMin, _UL_RMax, UL_H, UL_G} = reku(L),
                        {_UR_LMin, UR_RMax, UR_H, UR_G} = reku(R),
                        {UL_LMin, UR_RMax, H,
                            % Gueltigkeitspruefung
                            (UL_G and UR_G and is_number(W) and is_number(H) and (W > UL_LMin) and (W =< UR_RMax) and (my_max(UL_H, UR_H) + 1 == H))
                        };
% Alles andere is kein BTree
reku(_) -> {a, a, a, false}.




% Fuegt Wert in Binaerbaum ein

% Einfachster Fall: leerer Tree
insertBT({}, E) -> {E, {}, {}, 1};

% Fuege in Baum ein
insertBT({W, LTree, RTree, H}, Ele) ->
	case (Ele < W) of
            % Links eingehangen:
            true -> case (isEmptyBT(LTree)) of
                        % Linker Slot ist frei, haenge neues Blatt hier hin
                        % Wenn auf der anderen Seite was haengt, bleibt hoehe wie vorher
                        true when (H > 1) -> {W, {Ele, {}, {}, 1}, RTree, H};
                        % H ist 1 (also Blatt), darum wird die Hoehe um 1 erhoeht
                        true -> {W, {Ele, {}, {}, 1}, RTree, H + 1};

                        % linker Slot ist nicht frei, gib Element weiter runter und warte aufs die Hoehe des nachfolgenden Baumes
                        false -> {L_W, L_LT, L_RT, L_H} = insertBT(LTree, Ele),
                                    case (H > L_H) of
                                        % Wenn auf der anderen Seite ein laengerer Ast ist, bleibt H unberuehrt
                                        true -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H};
                                        % der linke Teilbaum wurde tiefer, die Hoehe muss um 1 erhoeht werden
                                        false -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H + 1}
                                    end
                     end;

            % Rechts eingehangen:
            false -> case (isEmptyBT(RTree)) of
                        true when (H > 1) -> {W, LTree, {Ele, {}, {}, 1}, H};
                        true -> {W, LTree, {Ele, {}, {}, 1}, H + 1};

                        false -> {R_W, R_LT, R_RT, R_H} = insertBT(RTree, Ele),
                                    case (H > R_H) of
                                        true -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H};
                                        false -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H + 1}
                                    end
                    end
	end.



% Prueft auf leeren Baum
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.



% Wert und Hoehe sind gleich, pruefe fuer linken und rechten Teilbaum rekursiv weiter
equalBT({W, LT1, RT1, H}, {W, LT2, RT2, H}) -> equalBT(LT1, LT2) and equalBT(RT1, RT2);
equalBT({}, {}) -> true;

% Alle anderen Faelle sind nicht gleich
equalBT(_BT1, _BT2) -> false.



% Hilfsfunktion
my_max(A, B) when A > B -> A;
my_max(_A, B) -> B.

testALL() ->   % Test Cases
                io:fwrite("TESTING ALL BTREE FUNCTIONS...~n~n"),

                % Test initBT/0
                TestResult_initBT = (
                                        btree:initBT() == {}
                                        ),

                case (TestResult_initBT) of
                    true -> io:fwrite("Test btree:initBT/0 - SUCCESSFUL~n");
                    false -> io:fwrite("Test btree:initBT/0 - FAILED~n")
                end,

                % Test isBT/1
                TestResult_isBT =       (
                                        btree:isBT({1,{},{},1}) and
                                        btree:isBT({1,{},{3,{},{},1},2}) and
                                        (btree:isBT({1,{},{},2}) == false) and
                                        (btree:isBT({b,{},{},a}) == false) and
                                        (btree:isBT({6,{},{},a}) == false) and
                                        (btree:isBT(btree) == false)
                                        ),
                case (TestResult_isBT) of
                    true -> io:fwrite("Test btree:isbT/2 - SUCCESSFUL~n");
                    false -> io:fwrite("Test btree:isbT/2 - FAILED~n")
                end,

                % Test insertBT/1
                X = btree:initBT(),
                X1 = btree:insertBT(X, 50),
                X2 = btree:insertBT(X1, 30),
                X3 = btree:insertBT(X2, 60),
                X4 = btree:insertBT(X3, 20),
                X5 = btree:insertBT(X4, 40),
                X6 = btree:insertBT(X5, 70),
                X7 = btree:insertBT(X6, 45),
                K = btree:insertBT(btree:insertBT(btree:insertBT(btree:insertBT(btree:insertBT(btree:insertBT(btree:insertBT(btree:insertBT(btree:initBT(),111),55),155),22),11),17),122),88),


                TestResult_insertBT =   (
                                        btree:isBT(X1)and
                                        btree:isBT(X2)and
                                        btree:isBT(X3)and
                                        btree:isBT(X4)and
                                        btree:isBT(X5)and
                                        btree:isBT(X6) and
                                        btree:isBT(X7) and
                                        btree:isBT(K)
                                        ),

                case (TestResult_insertBT) of
                    true -> io:fwrite("Test btree:insertBT/2 - SUCCESSFUL~n");
                    false -> io:fwrite("Test btree:insertBT/2 - FAILED~n")
                end,

                % Test isEmptyBT/1
                TestResult_isEmptyBT = (
                                        btree:isEmptyBT(btree:initBT()) and
                                        (btree:isEmptyBT(X1) == false)
                                        ),

                case (TestResult_isEmptyBT) of
                    true -> io:fwrite("Test btree:isEmptyBT/1 - SUCCESSFUL~n");
                    false -> io:fwrite("Test btree:isEmptyBT/1 - FAILED~n")
                end,

                % Test equalBT/2
                TestResult_equalBT = ((btree:equalBT(X7, X6) == false) and
                btree:equalBT(X7, X7)),
                case (TestResult_equalBT) of
                    true -> io:fwrite("Test btree:equalBT/2 - SUCCESSFUL~n");
                    false -> io:fwrite("Test btree:equalBT/2 - FAILED~n")
                end.

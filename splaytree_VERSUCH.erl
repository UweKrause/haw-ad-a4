-module(splaytree).

-author("Sean Pedersen").
-author("Uwe Krause").

%-export([initBT/0, isEmptyBT/1, equalBT/2, isBT/1, insertBT/2, deleteBT/2, findSBT/2, findBT/2, findTP/2, printBT/2]).

-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initBT: ∅ → btree / initBT()
% Stuetzt sich auf den Code aus Aufgabe 1
initBT() -> btree:initBT().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isEmptyBT: btree → bool / isEmptyBT(<BTree>)
% Stuetzt sich auf den Code aus Aufgabe 1
isEmptyBT(B) -> btree:isEmptyBT(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% equalBT: btree × btree → bool / equalBT(<BTree>,<BTree>)
% Stuetzt sich auf den Code aus Aufgabe 1
equalBT(B1, B2) -> btree:equalBT(B1, B2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isBT: btree → bool / isBT(<BTree>)
% Stuetzt sich auf den Code aus Aufgabe 1
isBT(B) -> btree:isBT(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insertBT: btree × elem → btree / insertBT(<BTree>,<Element>)
% Stuetzt sich auf den Code aus Aufgabe 1
insertBT(B, E) -> btree:insertBT(B, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deleteBT: btree × elem → btree
% Leicht abgeaendert aus AVL-Trees (Rotationen entfernt)

% Initiiere
% (Um ggf. zusaetzliche Rueckgabewerte zu filtern
deleteBT(B, Ele) -> deleteBT_reku(B, Ele).


% Baum wurde bis zum Ende durchlaufen,
% ohne dass der zu loeschende Wert gefunden wurde
% Wert ist nicht im Baum, Fehlerbehandlung durch ignorieren
deleteBT_reku({}, _Ele) -> {};

% Blatt
deleteBT_reku({ZuLoeschen, {}, {}, _H}, ZuLoeschen) -> {};

% Wert ist im aktuellem Knoten und es gibt keinen rechten Nachfolger
% Gib linken Nachfolger zurueck
deleteBT_reku(_B={W, L, {}, _H}, W) -> L;
  
% Wert ist im aktuellem Knoten und es gibt keinen linken Nachfolger
% Gib rechten Nachfolger zurueck
deleteBT_reku(_B={W, {}, R, _H}, W) -> R;
    
% Wert ist im aktuellem Knoten
% und es gibt sowohl rechten, als auch linken Nachfolger.
% Der inorderNachfolger wird ermittelt
deleteBT_reku(_B={W, L, R, _H}, W) ->
		
	% Minimalwert im Nachfolger finden und Nachfolger ohne diesen Wert
	{Ersatzwert, NeuRechts} = getMinimumAndRemove(R),
	
	% Wert durch Ersatzwert ersetzen
	% und Nachfolger durch reduzierten Nachfolger ersetzen
	{Ersatzwert, L, NeuRechts, avltree:get_hoehe(L, NeuRechts)};

% Rekursiver Abstieg nach links
deleteBT_reku(_B={W, L, R, _H}, ZuLoeschen) when ZuLoeschen < W ->
	NeuLinks = deleteBT_reku(L, ZuLoeschen),
	{W, NeuLinks, R, avltree:get_hoehe(NeuLinks, R)};
	
% Rekursiver Abstieg nach rechts
deleteBT_reku(_B={W, L, R, _H}, ZuLoeschen) when ZuLoeschen > W ->
	NeuRechts = deleteBT_reku(R, ZuLoeschen),
	{W, L, NeuRechts, avltree:get_hoehe(L, NeuRechts)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findSBT: btree × elem → int

% GEFUNDEN
findSBT(_B={E, _L, _R, H}, E) ->
    H;
    
% Baum komplett durchlaufen, Element nicht gefunden
% Fehlerwert: Position 0
findSBT({}, _E) -> 0;
    
    
% RECHTS
findSBT(_B={W, L, _R, _H}, E) when E < W ->
    % Der linke Unterbaum wird weiter durchsucht
    findSBT(L, E);
    
    
% LINKS
findSBT(_B={W, _L, R, _H}, E) when E > W ->    
    findSBT(R, E).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findBT: btree × elem → {int,btree}
% findet den Wert im Baum
% und rotiert ihn gemaess der Splaytree Regeln an die Wurzel des Gesamtbaums

% Die Wurzel ist bereits oberster Knoten
% (Sonderfall, keine Abbruchbedingung!)
findBT(B={E, _L, _R, H}, E) ->     
    {H, B};

    
% Funktionsstart, starte die Rekursion und extrahiere am Ende das Ergebnis   
findBT(B, E) -> 
    {B_neu={Bw, _Bl, _Br, Bh}, _} = findBT_(B, E, []),
    
    case Bw == E of
		% Erfolg, Element wurde gefunden
		true -> {Bh, B_neu};
		% Element wurde nicht gefunden, Fehlerwert ist 0
		% Baum wird unveraendert zurueck gegeben.
		% ("Fail silently")
		false -> {0, B_neu}
	end.
   
    
% GEFUNDEN
findBT_(B={E, _L, _R, _H}, E, PStack) ->
    % Hier wird zwar nicht tatsachlich gesplayt, aber die Stacks werden schon bearbeitet
    splay(B, PStack, []);
    

% Element nicht im Baum enthalten
%findBT_(B={}, _E, _PStack) ->
%	{B, []};
    

% Links weiter
findBT_(_B={W, L, R, H}, E, PStack) when E < W ->
    % Der linke Unterbaum wird weiter durchsucht
    %{L_neu, RStack} = findBT_(L, E, [l | PStack]),    
    
	case L == {} of
		true ->
			L_neu = {},
			RStack = [];
		false ->
			{L_neu, RStack} = findBT_(L, E, [l | PStack])
	end,
    
    % Der linke Unterbaum wurde ggf. durch die Suche veraendert
    B_neu = {W, L_neu, R, H},
    
    % Der resultierende Gesamtbaum wird (je nach Stacks) gesplayt
    splay(B_neu, PStack, RStack);
    
    
% Rechts weiter
findBT_(_B={W, L, R, H}, E, PStack) when E > W ->

	case R == {} of
		true ->
			R_neu = {},
			RStack = [];
		false ->
			{R_neu, RStack} = findBT_(R, E, [r | PStack])
	end,
    B_neu = {W, L, R_neu, H},
    splay(B_neu, PStack, RStack).
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findTP: btree × elem → {int,btree} 
% Findet den Wert im Baum und rotiert ihn um eine (1) Position nach oben.

% Die Wurzel ist bereits oberster Knoten
% (Sonderfall, keine Abbruchbedingung!)
findTP(B={E, _L, _R, H}, E) ->
    {H, B};
    
    
% Der gesuchte Wert befindet sich im linken Nachfolger
findTP(B={_W, _L={E, _LL, _LR, _LH}, _R, _H}, E) ->
	
    % Baum wird rotiert
    B_neu = {_W_neu, _L_neu, _R_neu, H_neu} = zig(B, r),
    
    % Rueckgabe setzt sich zusammen aus
    % der neuen Hoehe nach der Rotation und dem rotierten Unterbaum
    {H_neu, B_neu};
	
% Der gesuchte Wert befindet sich im rechten Nachfolger
findTP(B={_W, _L, _R={E, _RL, _RR, _RH}, _H}, E) ->
    B_neu = {_W_neu, _L_neu, _R_neu, H_neu} = zig(B, l),
    {H_neu, B_neu};
    
    
% Der Wert wird im linken Nachfolger weiter gesucht
findTP(_B={W, L, R, _H}, E) when E < W ->
    
    % Die Hoehe des gefundenen Knotens wird vom weiter nach oben gereicht
    {SH, L_neu} = findTP(L, E),    
    
    % Durch ggf modifizierten Teilbaum kann sich die Hoehe geaendert haben
    H_neu = avltree:get_hoehe(L_neu, R),
    
    % Aktueller Knoten wird mit Ergebnis der Rotation
    % und Hoehenaenderung wieder zusammengesetzt
    B_neu = {W, L_neu, R, H_neu},
    
    % Rueckgabe setzt sich zusammen aus
    % der zwischengespeicherten Hoehe und dem ggf. modifizierten Unterbaum
    {SH, B_neu};

% Der Wert wird im rechten Nachfolger weiter gesucht
findTP(_B={W, L, R, _H}, E) when E > W ->
    {SH, R_neu} = findTP(R, E),
    H_neu = avltree:get_hoehe(R_neu, L),
    B_neu = {W, L, R_neu, H_neu},
    {SH, B_neu};
    
    
% Wert nicht im Baum
% Fehlerwert ist Hoehe: 0
findTP({}, _E) ->
	{0, {}}.
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% printBT: btree × filename → dot / printBT(<BTree>,<Filename>)
% Datei mit gewuenschten Dateiname muss vorhanden und leer sein!

% ALIAS fuer Aufruf mit Standarddateiname
printBT(B) -> avltree:printBT(B, 'graph.dot').

% Reicht weiter an AVL-Tree
printBT(B, Filename) -> avltree:printBT(B, Filename).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hilfsfunktionen


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inorderNachfolger
% (OHNE ROTATION, deswegen NICHT einfach aus AVL Tree uebernehmen!)
getMinimumAndRemove({W, {}, R, _H}) -> {W, R};

getMinimumAndRemove(_B={W, L, R, _H}) ->
    {EntfernterWert, ResultierenderLinkerTeilbaum} = getMinimumAndRemove(L),
    NeuerBaum = {W, ResultierenderLinkerTeilbaum, R, avltree:get_hoehe(ResultierenderLinkerTeilbaum, R)},
    {EntfernterWert, NeuerBaum}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spreizen splay(B, PStack, RStack) -> {B_neu, RStack_neu}

splay(B, P, R) ->
    % Rekursionsstart und eventuell Platz fuer Debugausgaben
    splay_(B, P, R).
    

% Pfad leer
splay_(B, _Pfad=[], _Rueck=[]) ->
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {B, []};

splay_(B, _Pfad=[], _Rueck=[R1]) ->
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {rotiere(B, [R1]), []};

splay_(B, _Pfad=[], _Rueck=[R1, R2]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {rotiere(B, [R1, R2]), []};


% Pfad genau 1 Element
splay_(B, _Pfad=[P1], _Rueck=[]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {B, [P1]};

splay_(B, _Pfad=[P1], _Rueck=[R1]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {B, [P1, R1]};

splay_(B, _Pfad=[_P1], _Rueck=[R1, R2]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {rotiere(B, [R1, R2]), []};


% Pfad mindestens 2 Elemente
splay_(B, _Pfad=[P1, _P2 | _PR], _Rueck=[]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {B, [P1]};

splay_(B, _Pfad=[P1, _P2 | _PR], _Rueck=[R1]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {B, [P1, R1]};

splay_(B, _Pfad=[_P1, _P2 | _PR], _Rueck=[R1, R2]) -> 
    %io:format("[~w     |     ~w]~n",[Pfad, Rueck]),
    {rotiere(B, [R1, R2]), []}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ermittelt die notwendige Rotation anhand des Uebergabestacks.
% Ungueltige Eingaben werden nicht abgefangen!
%
% ACHTUNG, Zugriff auf als Stack genutzte Liste von Links nach rechts!

rotiere(B, [l]) ->
	io:format("Zig ~w~n", [rechts]),
	zig(B, r);

rotiere(B, [r]) -> 
	io:format("Zig ~w~n", [links]),
	zig(B, l);

rotiere(B, [l,l | _R]) -> 
	io:format("Zig-Zig ~w~n", [rechts]),
	zig_zig(B, r);

rotiere(B, [l,r | _R]) -> 
	io:format("Zig-Zag ~w~n", [rechts]),
	zig_zag(B, r);

rotiere(B, [r,l | _R]) -> 
	io:format("Zig-Zag ~w~n", [links]),
	zig_zag(B, l);

rotiere(B, [r,r | _R]) -> 
	io:format("Zig-Zig ~w~n", [links]),
	zig_zig(B, l).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fuehre die im vorherigen Schritt ermittelte Rotation durch.
% Ungueltige Eingaben werden nicht abgefangen!

% Eine einfache Rotation in die vorgegebene Richtung
zig(B, Richtung) ->
    
    case Richtung of
        % Nutzt die Rotationsfunktionen aus Aufgabe 3
        l -> avltree:rot_li(B);        
        r -> avltree:rot_re(B)
    end.
    
    
% Eine Zig-Zig Rotation besteht aus
% zwei hintereinander durchgefuehrten zig Rotationen in die gleiche Richtung
zig_zig(B, Richtung) ->
    B_neu = zig(zig(B, Richtung), Richtung),
    %%io:format("~nAlt: ~w~nNeu: ~w~n~n", [B, B_neu]),
    B_neu.


% Die zig-zag Operationen entspricht einer Ausführung einer zig Rotation
% in Gegenrichtung auf den jeweils anderen Unterbaum,
% gefolgt von einer zig Rotation in die angegebene Richtung
% auf den aktuell betrachteten Teilbaum,
% der den rotierten Unterbaum an entsprechender Stelle hat.
zig_zag(_B={W, L, R, _H}, Richtung) ->
    
    % Ermittel die Gegenrichtung
    case Richtung of 
        l -> Gegenrichtung = r;
        r -> Gegenrichtung = l
    end,
    
    % Rotiere den entsprechenden Unterbaum in die Gegenrichtung
    % und anschliessend den entstehenden Baum mit bereits rotierten Unterbaum
    % in die angegebene Richtung
    case Richtung of
        l -> 
            R_neu = zig(R, Gegenrichtung),
            B_neu = zig({W, L, R_neu, avltree:get_hoehe(L, R_neu)}, Richtung);
        r ->
            L_neu = zig(L, Gegenrichtung),
            B_neu = zig({W, L_neu, R, avltree:get_hoehe(L_neu, R)}, Richtung)
    end,
    
    %io:format("~nAlt: ~w~nNeu: ~w~n~n", [B, B_neu]),
    B_neu.

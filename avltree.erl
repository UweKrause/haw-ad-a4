-module(avltree).

-author("Sean Pedersen").
-author("Uwe Krause").

-export([initBT/0, isEmptyBT/1, equalBT/2, isBT/1, insertBT/2, deleteBT/2, printBT/2]).

% Ist fuer die unittests noch drin!
-compile(export_all).




%initBT: ∅ → btree / initBT()
initBT() -> {}.



%isEmptyBT: btree → bool / isEmptyBT(<BTree>)
isEmptyBT({}) -> true;

isEmptyBT(_) -> false.




%equalBT: btree × btree → bool / equalBT(<BTree>,<BTree>)

% Wert und Hoehe sind gleich, pruefe fuer linken und rechten Teilbaum rekursiv weiter
equalBT({W, LT1, RT1, H}, {W, LT2, RT2, H}) -> equalBT(LT1, LT2) and equalBT(RT1, RT2);
equalBT({}, {}) -> true;

% Alle anderen Faelle sind nicht gleich
equalBT(_BT1, _BT2) -> false.




%isBT: btree → bool / isBT(<BTree>)
isBT(B) -> {_LMin, _RMax, _H, Gueltigkeit} = isBT_reku(B), (Gueltigkeit or isEmptyBT(B)).

% Rueckgabe-Tupel: {Lmin, Rmax, Hoehe, Gueltigkeit}

% Blatt
isBT_reku({W, {}, {}, H}) -> {W, W, H, is_number(W) and is_number(H) and (H == 1)};

% Links weiter
isBT_reku({W, L, {}, H}) ->  {ULMin, _URMax, UH, UG} = isBT_reku(L),
                        {ULMin, W, H, (UG and is_number(W) and is_number(H) and (W > ULMin) and (UH + 1 == H)
                        and (UH >= -1) and (UH =< 1))};


% Rechts weiter
isBT_reku({W, {}, R, H}) ->  {_ULMin, URMax, UH, UG} = isBT_reku(R),
                        {W, URMax, H, (UG and is_number(W) and is_number(H) and (W =< URMax) and (UH + 1 == H)
                        and (UH >= -1) and (UH =< 1))};

% Beide weiter
isBT_reku({W, L, R, H}) ->   {UL_LMin, _UL_RMax, UL_H, UL_G} = isBT_reku(L),
                        {_UR_LMin, UR_RMax, UR_H, UR_G} = isBT_reku(R),
                        {UL_LMin, UR_RMax, H,
                            % Gueltigkeitspruefung
                            (UL_G and UR_G and is_number(W) and is_number(H) and (W > UL_LMin) and (W =< UR_RMax) and (max(UL_H, UR_H) + 1 == H)
                            and (UR_H-UL_H >= -1) and (UR_H-UL_H =< 1))
                        };

% Alles andere is kein BTree
isBT_reku(_) -> {a, a, a, false}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insertBT
% insertBT: btree × elem → btree / insertBT(<BTree>,<Element>)
%
% Wie in [Weiker, Weiker]
% mit Zwischenspeicherung der Hoehenaenderung
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starte Rekursion
insertBT(B, E) -> 
	% Zaehler initiieren
	util:countreset(leftrotate),
	util:countreset(rightrotate),
	util:countreset(ddleftrotate),
	util:countreset(ddrightrotate),
	
	% Rekursion starten
    {ResultierenderBaum, _GleicheHoehe} = insertBT_reku(B, E),
    
    
    ResultierenderBaum.


% Rekursionsabbruch, leerer Baum zum Einfuegen gefunden
insertBT_reku({}, E) -> 
    { _Baum={E, avltree:initBT(), avltree:initBT(), 1}, _GleicheHoehe = false};

% Einzufuegender Wert kleiner -> links weiter
insertBT_reku(_B={W, L, R, _H}, E) when (E < W) ->
    %io:format("links~n", []),
    {EingefuegterTeilbaum, GleicheHoehe} = insertBT_reku(L, E),
    pruefe_rot_re({W, EingefuegterTeilbaum, R, get_hoehe(EingefuegterTeilbaum, R)}, GleicheHoehe);

    
% Einzufuegender Wert groesser -> rechts weiter
insertBT_reku(_B={W, L, R, _H}, E) when (E > W) ->
    %io:format("rechts~n", []),
    {EingefuegterTeilbaum, GleicheHoehe} = insertBT_reku(R, E),
    pruefe_rot_li({W, L, EingefuegterTeilbaum, get_hoehe(EingefuegterTeilbaum, L)}, GleicheHoehe);

    
% Fehlerfall, Element bereits enthalten.
insertBT_reku(Baum={E, _L, _R, _H}, E) -> 
    {Baum, _GleicheHoehe = true}.
 
 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rotationsueberpruefung
% wie in [Weiker, Weiker]
% mit Zwischenspeicherung der Hoehenaenderung
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Keine Rotation notwendig, weil im vorhergehenden Schritt keine Hoehenaenderung
pruefe_rot_re(B, _GleicheHoehe = true) -> 
    %io:format("keine Aenderung~n", []),
    {B, true};

% im vorhergehenden Schritt gab es eine Hoehenaenderung, ggf. ist eine Rotation notwendig
pruefe_rot_re(B, _GleicheHoehe = false) ->
    case get_balance(B) of
        1 -> {B, false};
        0 -> {B, true};
        -1 -> {B, false};
        -2 -> reorg_rot_re(B)
    end.


    
% Keine Rotation notwendig, weil im vorhergehenden Schritt keine Hoehenaenderung
pruefe_rot_li(B, _GleicheHoehe = true) -> {B, true};

% im vorhergehenden Schritt gab es eine Hoehenaenderung, ggf. ist eine Rotation notwendig
pruefe_rot_li(B, _GleicheHoehe = false) ->
    case get_balance(B) of
        -1 -> {B, false};
        0 -> {B, true};
        1 -> {B, false};
        2 -> reorg_rot_li(B)
    end.
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reorganisation durch Rotation
% wie in [Weiker, Weiker]
% mit Zwischenspeicherung der Hoehenaenderung
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
reorg_rot_re(B={_W, L, _R, _H}) ->
           
	case get_balance(L) of
        -1 -> {rot_re(B), false};
        0 -> {rot_re(B), false}; % Nur beim loeschen relevant
        1 -> {doppel_rot_re(B), true} % Doppelrotation
    end.

    
  
reorg_rot_li(B={_W, _L, R, _H}) ->

	case get_balance(R) of
        1 -> {rot_li(B), false};
        0 -> {rot_li(B), false}; % Nur beim loeschen relevant
        -1 -> {doppel_rot_li(B), true} % Doppelrotation
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deleteBT
% Wie in Vorlesung vorgestellt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initiiere
% (Um ggf. zusaetzliche Rueckgabewerte zu filtern
deleteBT(B, Ele) -> deleteBT_reku(B, Ele).


% Baum wurde bis zum Ende durchlaufen, ohne dass der zu loeschende Wert gefunden wurde
% Wert ist nicht im Baum, Fehlerbehandlung durch ignorieren
deleteBT_reku({}, _Ele) -> {};


% Blatt
deleteBT_reku({ZuLoeschen, {}, {}, _H}, ZuLoeschen) -> {};


% nur linker Unterbaum
deleteBT_reku(_B={W, L, {}, _H}, W) ->
	balance(L);

  
% nur rechter Unterbaum
deleteBT_reku(_B={W, {}, R, _H}, W) ->
	balance(R);
  
  
% beide Unterbeaume
deleteBT_reku(_B={W, L, R, _H}, W) ->
		
	% Minimalwert im Nachfolger finden und nachfolger ohne diesen Wert
	{Ersatzwert, NeuRechts} = getMinimumAndRemove(R),
	
	% Wert durch Ersatzwert ersetzen
	% und Nachfolger durch redizierten Nachfolger ersetzen
	Unbalanciert = {Ersatzwert, L, NeuRechts, get_hoehe(L, NeuRechts)},
	
	% Das Ergebnis muss ggf. wieder balanciert werden!
	balance(Unbalanciert);

  
% Rekursiver Abstieg nach links
deleteBT_reku(_B={W, L, R, _H}, ZuLoeschen) when ZuLoeschen < W ->
	NeuLinks = deleteBT_reku(L, ZuLoeschen),
	Unbalanciert = {W, NeuLinks, R, get_hoehe(NeuLinks, R)},
	balance(Unbalanciert);

	
% Rekursiver Abstieg nach rechts
deleteBT_reku(_B={W, L, R, _H}, ZuLoeschen) when ZuLoeschen > W ->
	NeuRechts = deleteBT_reku(R, ZuLoeschen),
	Unbalanciert = {W, L, NeuRechts, get_hoehe(L, NeuRechts)},
	balance(Unbalanciert).

    
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% printBT: btree × filename → dot / printBT(<BTree>,<Filename>)
% Datei mit gewuenschten Dateiname muss vorhanden und leer sein!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


printBT({}, Filename) ->
% der Leere Baum wird als Knoten ohne Wert und Nachfolger angezeigt
	util:logging(Filename, io_lib:format("digraph avltree { \"\"; }", []));



% Fuer einen nicht-leeren Knoten wird die Syntax einer graphViz Datei vorbereitet
printBT(BT, Filename) ->
	printBT_header(Filename),
	printBT_body(BT, Filename).


printBT_header(Filename) ->
	% Einleitdende Metadaten, die dem GrapvViz Programm sagen, welche Art von Diagramm erstellt werden soll und wie es heisst
	util:logging(Filename, io_lib:format("digraph avltree ", [])).


printBT_body(BT, Filename) ->
	% Einleitung des Blocks
	util:logging(Filename, io_lib:format("{~n~n", [])),
	% Rekursive Iteration durch Baum mit Ausgabe der Struktur in Datei
	printBT_body_info(BT, Filename),
	% Abschluss des Blocks
	util:logging(Filename, io_lib:format("}~n", [])).


% leerer Baum wird angezeigt.
% (Dies ist ein Sonderfall, aber nicht die Abbruchbedingung!)
printBT_body_info({}, Filename) ->
	util:logging(Filename, io_lib:format("\"\" -> \"\";~n", []));



%%%%%
% Der Baum ist nicht leer und wird durchlaufen wobei für jeden Knoten der Wert und die jeweiligen Nachfolger mit der Höhe der Nachfolger ausgegeben werden.
% Ausgabereihenfolge ist dabei von oben nach unten, von links nach rechts.
% (Fuer GraphViz ist die Reihenfolge egal, das Programm strkturiert sich das selber richtig hin.)
%%%%%

% beide Nachfolger vorhanden
printBT_body_info({W, L={LW, _LL, _LR, LH}, R={RW, _RL, _RR, RH}, _H}, Filename) ->
	util:logging(Filename, io_lib:format("~w -> ~w [label = ~w];~n", [W, LW, LH])),
	util:logging(Filename, io_lib:format("~w -> ~w [label = ~w];~n~n", [W, RW, RH])),
	printBT_body_info(L, Filename),
	printBT_body_info(R, Filename);

% nur der rechte Nachfolger vorhanden
printBT_body_info({W, {}, R={RW, _RL, _RR, RH}, _H}, Filename) ->
        util:logging(Filename, io_lib:format("//rechts~n", [])),
	util:logging(Filename, io_lib:format("~w -> ~w [label = ~w];~n~n", [W, RW, RH])),
	printBT_body_info(R, Filename);

% Nur der linke Nachfolger vorhanden
printBT_body_info({W, R={LW, _LL, _LR, LH}, {}, _H}, Filename) ->
	util:logging(Filename, io_lib:format("//links~n", [])),
	util:logging(Filename, io_lib:format("~w -> ~w [label = ~w];~n~n", [W, LW, LH])),
	printBT_body_info(R, Filename);

% Keine Nachfolger mehr, Abbruchbedingung
printBT_body_info({_W, {}, {}, _H}, Filename) ->
% 	% Das Schreiben in Dateien kann in einem anderen Prozess passieren
% 	% Eine Konsolenausgabe eines Atoms koennte die Anzeigereihenfolge auf der Konsole verruecken, deswegen leere Ausgabe
	util:logging(Filename, io_lib:format("", [])).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hilfsfunktionen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Kein kleinerer Wert mehr
% entferne Wert
getMinimumAndRemove({W, {}, R, _H}) -> {W, R};


getMinimumAndRemove(_B={W, L, R, _H}) ->
    {EntfernterWert, ResultierenderLinkerTeilbaum} = getMinimumAndRemove(L),
    
    NeuerBaum = {W, ResultierenderLinkerTeilbaum, R, get_hoehe(ResultierenderLinkerTeilbaum, R)},
    
    GgfRotierterBaum = balance(NeuerBaum),
    
    {EntfernterWert, GgfRotierterBaum}.




%%%%%%%%%%%%%%%
% Die Balance ist die Differenz der Hoehe des rechten und des linken Teilbaums 
%%%%%%%%%%%%%%%

% Lerer Baum hat Balance 0
get_balance({}) -> 0;

% Baum ohne Nachfolger hat Balance 0 - 0 = 0
get_balance(_B={_W, {}, {}, _H}) -> 0;

% Linker Nachfolger ist Leer 
% Rechte Hoehe - 0 = Rechte Hoehe
get_balance(_B={_W, {}, {_RW, _RL, _RR, RH}, _H}) -> RH;

% Rechter Nachfolger ist Leer 
% 0 - linke Hoehe 
% ACHTUNG! Nicht linke Hoehe, sondern 0 minus linke Hoehe!
get_balance(_B={_W, {_LW, _LL, _LR, LH}, {}, _H}) -> 0 - LH;

% Es existieren zwei Nachfolger
% Balance ist rechte Hoehe - linke Hoehe
get_balance(_B={_W, {_LW, _LL, _LR, LH}, {_RW, _RL, _RR, RH}, _H}) -> RH - LH.
    


%%%%%%%%%%%%%%%
% Balancierung
%%%%%%%%%%%%%%%

balance(UnbalancedTree={_W, L, R, _H}) ->
	% Balance ist HoeheRechts - HoeheLinks
	% Balanciert sind -1, 0 , 1
	% Unbalanciert sind -2, 2
	% (Siehe Tabelle in Entwurf)

	Balance = get_balance(UnbalancedTree),
	
	case Balance of
		% Baum ist bereits balanciert
		0 -> UnbalancedTree;
		-1 -> UnbalancedTree;
		1 -> UnbalancedTree;

		% Baum ist unbalanciert und muss durch Rotation neu organisiert werden
		2 ->
			BalanceRight = get_balance(R),
			case BalanceRight of
				1 -> rot_li(UnbalancedTree);
				0 -> rot_li(UnbalancedTree);
				-1 -> doppel_rot_li(UnbalancedTree)
			end;
			
		-2 ->
			BalanceLeft = get_balance(L),
			case BalanceLeft of
				-1 -> rot_re(UnbalancedTree);
				0 -> rot_re(UnbalancedTree);
				1 -> doppel_rot_re(UnbalancedTree)
			end
	end.

	
	
%%%%%%%%%%%%%%%
% Rotationen
% Ungueltige Eingaben werden nicht abgefangen!
%%%%%%%%%%%%%%%

% Rotation nach Links
rot_li(_B={W, L, _R={Rw, RL, RR, _Rh}, _H}) ->
    util:counting1(leftrotate),
	
	NeuLinks = {W, L, RL, get_hoehe(L, RL)},
    {Rw, NeuLinks, RR, get_hoehe(NeuLinks, RR)}.

    
% Doppelrotation nach Links ist erst eine Rechtsrotation des rechten Nachfolgers,
% dann eine Linksrotation des Gesamten, inklusive des rotierten Nachfolgers
doppel_rot_li(_B={W, L, R, H}) ->
	util:counting1(ddleftrotate),

	% Die Hoehe veraendert sich nach einer (DOPPEL!)Rotation nicht, muss also auch nicht neu berechnet werden
    rot_li({W, L, rot_re(R), H}).

    
% Rotation nach rechts
rot_re(_B={W, _L={Lw, LL, LR, _Lh}, R, _H}) ->
	util:counting1(rightrotate),
	
    NeuRechts = {W, LR, R, get_hoehe(LR, R)},
    {Lw, LL, NeuRechts, get_hoehe(LL, NeuRechts)}.

    
% Doppelrotation nach Rechts ist erst eine Linksrotation des linken Nachfolgers,
% dann eine Rechtssrotation des Gesamten, inklusive des rotierten Nachfolgers
doppel_rot_re(_B={W, L, R, H}) ->
	util:counting1(ddrightrotate),
	
	rot_re({W, rot_li(L), R, H}).
  
  
  
%%%%%%%%%%%%%%%
% Ermittel die Hoehe Auf Grundlage der Nachfolgerhoehen
%%%%%%%%%%%%%%%
get_hoehe(L, R) -> max_hoehe(L,R) + 1.

%%%%%%%%%%%%%%%
% Maximalhoehe der beiden Nachfolger
%%%%%%%%%%%%%%%
max_hoehe(_L={}, _R={}) -> 0;

max_hoehe(_L={}, _R={_RW, _RL, _RR, RH}) -> RH;

max_hoehe(_L={_LW, _LL, _LR, LH}, _R={}) -> LH;

max_hoehe(_L={_LW, _LL, _LR, LH}, _R={_RW, _RL, _RR, RH}) -> max(LH, RH).

% Implementieren Sie eine Testumgebung, in der Sie die Laufzeit messen können.
% Um viele Zahlen verwenden zu können, nutzen Sie die Funktion randomliste/1
% aus der Datei util.erl. Implementieren Sie zudem einen Test, der bei der Suche
% nach dem selben Element die Laufzeiten von findBT und findTP vergleicht.
% Der Baum ist jeweils am Ende des Tests mittels printBT auszugeben.
% Dokumentieren und interpretieren Sie die Ergebnisse.

-module(testumgebung).

-author("Sean Pedersen").
-author("Uwe Krause").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% messen der Laufzeiten

% ALIAS fuer Defaulwerte
testen() -> testen(100, 10000, 95, 100).

testen(AnzahlTestdaten, AnzahlSuchvorgaenge, VON, BIS) ->

    % Generieren einer zufällig sortierten Liste mit X unterschiedlichen Elementen OHNE Duplikate
    {ZeitTestListe, Testliste} = timer:tc(util, randomliste, [AnzahlTestdaten]),
    %{ZeitTestListe, Testliste} = timer:tc(lists,seq, [1, AnzahlTestdaten]),

    % Diese Liste in einen Baum einspeisen
    LeererBaum = splaytree:initBT(),
    {ZeitTestbaum, Baum} = timer:tc(testumgebung, einfuegen, [LeererBaum, Testliste]),
    
    
    % Baum kann fuer Debugzwecke angezeigt werden
    % ACHTUNG, .dot DATEI VORHER PER HAND LEEREN!
    %splaytree:printBT(Baum, 'testbaum.dot'),
    
    % generieren einer unsortierten Liste von Elementen mit Duplikaten, die im Baum gesucht werden soll
    % Anzahl Elemente im Bereich von VON bis BIS
    {ZeitSuchliste, Suchliste} = timer:tc(util, randomlisteD, [AnzahlSuchvorgaenge, VON, BIS]),
    
    % Suchliste kann fuer Debugzwecke ausgegeben werden
    %io:format("Suchliste:~n~w~n", [Suchliste]),
    
    % Suche der Elemente im Baum,
    % jeweils mit den unterschiedlichen Optimierungsstrategien
    % und Ausgabe der Zeitmessungen
    
    io:format("Dauer der Suche von ~w Elementen in einem Baum mit ~w Knoten:~n", [AnzahlSuchvorgaenge, AnzahlTestdaten]),
    
    % keine Optimierungsstrategie
    io:format("Ohne ~w mikrosecs~n", [suchen(Baum, Suchliste, findSBT)]),
    
    % Splay-to-front
    io:format("MoveToFront ~w mikrosecs~n", [suchen(Baum, Suchliste, findBT)]),
    
    % Transpose
    io:format("Transpose ~w mikrosecs~n", [suchen(Baum, Suchliste, findTP)]),
    
    % Ausgabe der benoetigten Zeit zur Generierung der AnzahlTestdaten
    io:format(
		"~nGenerierung der Testdaten [Mikrosekunden]:~nTestliste: ~w TestBaum: ~w Suchliste: ~w~n",
		[ZeitTestListe, ZeitTestbaum, ZeitSuchliste]
	).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generiert aus einer Liste einen Baum
einfuegen(B, List) -> ein_reku(B, List).

% Abbruch, Liste ist leer, Baum wird rekursiv rueckwaerts aufgebaut
ein_reku(B, []) -> B;

% Der Liste wird das oberste Element entfernt und in den Baum einsortiert
ein_reku(Baum, [H|T]) ->
	ResultTree = splaytree:insertBT(Baum, H),
	ein_reku(ResultTree, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sucht in einem Baum eine Liste von Elementen und zaehlt die benoetigte Zeit zusammen
suchen(B, List, FindOperation) -> suchen_(B, List, FindOperation, 0).

% Ende der Liste erreicht, gib benoetigte Gesamtzeit zurueck
suchen_(_B, [], _FindOperation, Time) -> Time;

% Es sind weitere zu suchende Werte in der Liste:
% such nach dem ersten Wert,
% miss die Zeit
% und suche den Rest im ggf. modifizierten Baum
suchen_(Baum, [Head|Rest], FindOperation, Time) ->
    {MessTime, ResultTree} = timer:tc(testumgebung, FindOperation, [Baum, Head]),
    suchen_(ResultTree, Rest, FindOperation, (Time+MessTime)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% manipulieren der Rueckgabeparameter der Suchoperationen

% Die Hoehe interessiert nicht, aber der entstehende Baum
findBT(B, E) ->
    {_Hoehe, Resulttree} = splaytree:findBT(B, E),
    Resulttree.
    
% Die Hoehe interessiert nicht, aber der Baum muss weiter gegeben werden
findSBT(B, E) ->
    splaytree:findSBT(B, E),
    % Der Baum wird bei der Suche nicht veraendert
    B.
    
% Die Hoehe interessiert nicht, aber der entstehende Baum    
findTP(B, E) ->
    {_Hoehe, Resulttree} = splaytree:findTP(B, E),
    Resulttree.
    
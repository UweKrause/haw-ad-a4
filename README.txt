Da sich der Splaytree auf Funktionen aus den ersten beiden Aufgaben stuetzt,
sind diese Module beigelegt und muessen vor der Ausfuehrung ebenfalls
kompiliert werden!

Befehlsliste zum kopieren:


c(splaytree).
c(btree).
c(avltree).
c(util).
c(testumgebung).


Die Testumgebung befindet sich in der Datei testumgebung.erl

Gestartet wird der Test mit Defaultwerten:
testumgebung:testen().


Ansonsten lassen sich die Werte manuell anpassen:
testumgebung:testen(AnzahlTestdaten, AnzahlSuchvorgaenge, VON, BIS).

Wobei:
	- AnzahlTestdaten die Anzahl der Elemente ist, die in den Baum eingefuegt werden
	- AnzahlSuchvorgaenge die Anzahl der im Baum zu suchenden Elemente ist
	- und VON und BIS die Bandbreite der zu suchenden Zahlen definiert
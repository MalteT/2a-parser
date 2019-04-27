#! mrasm ;Kennung für den Assembler, muss in erster Zeile stehen!

;Rampe auf DAC1 ausgeben:
  .ORG 0 ;bei Adresse 0 anfangen

	CLR R0 ;Anfangswert
LOOP: ;Anfang Schleife (Label muss in extra Zeile stehen)
	ST (0xFF),R0 ;Wert anzeigen
	ST (0xF0),R0 ;Wert auf DAC1
	INC R0
	JR LOOP ;Endlosschleife

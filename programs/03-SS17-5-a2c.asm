#! mrasm

.ORG 0

;;; Ruft Subroute zur Bestimmung der Temperatur auf
;;; und vergleicht diesen Wert mit einer
;;; unteren Grenze (0xFE) und
;;; oberen Grenze (0xFF)
MAIN:
  CALL SUBR                     ;Rufe Subroutine auf
  ST (0xFE), R0
  CMP R0, (0xFE)                ;Vergleiche untere Grenze mit Ergebnis
  JCS DRUNTER                   ;Falls untere Grenze > Ergebnis
  CMP (0xFF), R0                ;Vergleiche obere Grenze mit Ergebnis
  JCS DRINN                     ;Falls untere Grenze <= Ergebnis < obere Grenze
DRUEBER:
  MOV (0xF0), (0xFC)            ;Lade (0xFC) nach DAC1
  MOV (0xFF), (0xFC)
  JR MAIN
DRUNTER:
  MOV (0xF0), (0xFD)            ;Lade (0xFD) nach DAC1
  MOV (0xFF), (0xFD)
  JR MAIN
DRINN:
  MOV (0xF0), 0                 ;Lade 0 nach DAC1
  MOV (0xFF), 0
  JR MAIN

;;; Subroutine:
;;; Ermittle mittels sukzessiver Approximation
;;; die Spannung am Temperatursensor.
SUBR:                           ;Start der Subroutine
  CLR R0                        ;Testwert
  LD R1, 0b10000000             ;Bitmaske für das zu setzende Bit
  LD R2, 0b00010000             ;Bitmaske für F1 (Bit 4)
SUKZ:                           ;Sukzessive Approximation
  OR R0, R1                     ;Füge das Bit von der Bitmaske hinzu
  ST (0xF1), R0                 ;Schicke den Testwert an DAC2
  BITT (0xF1), R2               ;Prüfe Bit 4 von (F1)
  JZC KEINMINUS                 ;Springe falls, Bit bleiben muss
  XOR R0, R1                    ;Entferne das Bit
KEINMINUS:
  LSR R1                        ;Shifte die Bitmaske nach rechts
  JZS SUBENDE                   ;Springe zum Ende der Subroutine, falls Bitmaske 0
  JR SUKZ                       ;Beginne von vorn
SUBENDE:                        ;Ende der Routine
  RET                           ;Beende die Subroutine und springe zum Caller

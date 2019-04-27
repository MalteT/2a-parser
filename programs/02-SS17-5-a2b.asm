#! mrasm

.ORG 0

;;; Rufe die Subroutine in Dauerschleife auf
;;; und schreibe das Ergebnis in (FF) und (F0).
MAIN:
  CALL SUBR                     ;Rufe die Subroutine auf
  ST (0xFF), R0                 ;Schreibe das Ergebnis der Subroutine
  ST (0xF0), R0                 ;in (FF),(F0)
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

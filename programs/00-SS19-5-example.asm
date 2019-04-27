#! mrasm

    .ORG 0

    CLR R0
LOOP:
    ST (0xFF), R0
    ST (0xF0), R0
    INC R0
    JR LOOP

       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO TRANSIN
               ORGANIZATION IS LINE SEQUENTIAL
       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS TRANS-RECORD.
       01  TRANS-RECORD           PIC X(80).

       WORKING-STORAGE SECTION.
       77  WS-EOF                 PIC X VALUE 'N'.
       77  WS-TOTAL               PIC 9(5) VALUE 0.
       77  WS-VALID               PIC 9(5) VALUE 0.
       77  WS-INVALID             PIC 9(5) VALUE 0.

       01  WS-TX-FIELDS.
           05  WS-TXN-ID          PIC X(6).
           05  FILLER             PIC X VALUE ','.
           05  WS-TXN-TYPE        PIC X(10).
           05  FILLER             PIC X VALUE ','.
           05  WS-ACC-ID          PIC X(5).
           05  FILLER             PIC X VALUE ','.
           05  WS-AMOUNT          PIC X(8).
           05  FILLER             PIC X VALUE ','.
           05  WS-DATE            PIC X(8).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "BATCH-VALIDATOR: Starting transaction validation...".
           OPEN INPUT TRANS-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ TRANS-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-TOTAL
                       MOVE TRANS-RECORD TO WS-TX-FIELDS
                       PERFORM VALIDATE-TXN
               END-READ
           END-PERFORM
           CLOSE TRANS-FILE
           DISPLAY "BATCH-VALIDATOR: Validation completed"
           DISPLAY "BATCH-VALIDATOR: Total transactions: " 
                   WS-TOTAL UPON CONSOLE
           DISPLAY "BATCH-VALIDATOR: Valid transactions: " 
                   WS-VALID UPON CONSOLE
           DISPLAY "BATCH-VALIDATOR: Invalid transactions: " 
                   WS-INVALID UPON CONSOLE
           IF WS-INVALID > 0
               DISPLAY "BATCH-VALIDATOR: Invalid transactions found!"
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF
           STOP RUN.

       VALIDATE-TXN.
           IF WS-TXN-TYPE = "DEPOSIT   "
             OR WS-TXN-TYPE = "WITHDRAWAL"
             OR WS-TXN-TYPE = "TRANSFER  "
               ADD 1 TO WS-VALID
               DISPLAY "✓ VALID: " TRANS-RECORD
           ELSE
               ADD 1 TO WS-INVALID
               DISPLAY "✗ INVALID: " TRANS-RECORD
           END-IF.

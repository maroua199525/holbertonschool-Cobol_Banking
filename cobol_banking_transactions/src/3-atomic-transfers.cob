IDENTIFICATION DIVISION.
       PROGRAM-ID. transfer-processor.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSFER-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TRANSFER-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSFER-FILE.
       01  TRANSFER-RECORD      PIC X(200).
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  DB-CONN-STRING       PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  TRANSFER-FILE-STATUS PIC X(2).
       01  TRANSFER-INFO.
           05 OPERATION-TYPE    PIC X(8).
           05 SOURCE-ACCOUNT    PIC X(4).
           05 TARGET-ACCOUNT    PIC X(4).
           05 TRANSFER-AMOUNT   PIC X(10).
       01  WITHDRAW-RESULT      PIC S9(9) COMP-5.
       01  DEPOSIT-RESULT       PIC S9(9) COMP-5.
       
       PROCEDURE DIVISION.
       START-PROCESSING.
           MOVE FUNCTION TRIM(DB-CONN-STRING) TO DB-CONNSTR.
           
           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.
           
           OPEN INPUT TRANSFER-FILE.
           PERFORM READ-TRANSFERS UNTIL TRANSFER-FILE-STATUS NOT = "00".
           CLOSE TRANSFER-FILE.
           
           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.
           GOBACK.
       
       READ-TRANSFERS.
           READ TRANSFER-FILE AT END MOVE "10" TO TRANSFER-FILE-STATUS.
           IF TRANSFER-FILE-STATUS = "00" THEN
               UNSTRING TRANSFER-RECORD DELIMITED BY ","
                   INTO OPERATION-TYPE, SOURCE-ACCOUNT, 
                        TARGET-ACCOUNT, TRANSFER-AMOUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(OPERATION-TYPE)) = "TRANSFER"
                   PERFORM EXECUTE-TRANSFER
               END-IF
           END-IF.
       
       EXECUTE-TRANSFER.
           CALL STATIC "DB_BEGIN" 
               USING BY VALUE DBH 
               RETURNING RC.
           
           IF RC NOT = 0 THEN
               DISPLAY "ERROR: Could not begin transaction."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE SPACES TO SQL-COMMAND.
           STRING "UPDATE accounts SET balance = balance - "
               FUNCTION TRIM(TRANSFER-AMOUNT) " WHERE account_id = "
               FUNCTION TRIM(SOURCE-ACCOUNT) ";"
               DELIMITED BY SIZE INTO SQL-COMMAND.
           
           CALL STATIC "DB_EXEC" 
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING WITHDRAW-RESULT.
           
           MOVE SPACES TO SQL-COMMAND.
           STRING "UPDATE accounts SET balance = balance + "
               FUNCTION TRIM(TRANSFER-AMOUNT) " WHERE account_id = "
               FUNCTION TRIM(TARGET-ACCOUNT) ";"
               DELIMITED BY SIZE INTO SQL-COMMAND.
           
           CALL STATIC "DB_EXEC" 
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING DEPOSIT-RESULT.
           
           IF WITHDRAW-RESULT = 0 AND DEPOSIT-RESULT = 0 THEN
               CALL STATIC "DB_COMMIT" 
                   USING BY VALUE DBH 
                   RETURNING RC
               DISPLAY "SUCCESS: Transfer of " FUNCTION TRIM(TRANSFER-AMOUNT)
                       " from " FUNCTION TRIM(SOURCE-ACCOUNT)
                       " to " FUNCTION TRIM(TARGET-ACCOUNT) " committed."
           ELSE
               CALL STATIC "DB_ROLLBACK" 
                   USING BY VALUE DBH 
                   RETURNING RC
               DISPLAY "FAILURE: Transfer rolled back."
           END-IF.

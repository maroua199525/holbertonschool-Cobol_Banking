IDENTIFICATION DIVISION.
       PROGRAM-ID. audit-trail.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TX-FILE ASSIGN TO "transactions.dat"
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS TX-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TX-FILE.
       01  TX-RECORD                 PIC X(200).

       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT                  PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L                         PIC 9(4) VALUE 0.
       01  TX-FILE-STATUS            PIC XX VALUE "00".
       01  SQL-LIT                   PIC X(200).

       01  TX-DATA.
           05 TX-ACTION              PIC X(20).
           05 TX-ACCOUNT-ID         PIC X(20).
           05 TX-AMOUNT             PIC X(30).

       01  CURRENT-BALANCE           PIC S9(9)V99 VALUE 0.
       01  WITHDRAWAL-AMOUNT         PIC S9(9)V99 VALUE 0.
       01  WS-BALANCE                PIC X(20).
       01  WS-AMOUNT                 PIC X(10).

       01  DONE                      PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT))
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L)
           MOVE X"00" TO DB-CONNSTR(L + 1:1)
           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH
           IF DBH = NULL-PTR
              DISPLAY "ERROR: Database connection failed"
              STOP RUN
           END-IF

           OPEN INPUT TX-FILE
           PERFORM UNTIL DONE = "Y"
              READ TX-FILE
                 AT END
                    MOVE "Y" TO DONE
                 NOT AT END
                    PERFORM PARSE-LINE
                    IF FUNCTION UPPER-CASE(FUNCTION TRIM(TX-ACTION)) = "WITHDRAW"
                       PERFORM VALIDATE-AND-PROCESS
                    END-IF
              END-READ
           END-PERFORM
           CLOSE TX-FILE

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC
           GOBACK.

       PARSE-LINE.
           MOVE SPACES TO TX-ACTION
           MOVE SPACES TO TX-ACCOUNT-ID
           MOVE SPACES TO TX-AMOUNT
           UNSTRING TX-RECORD
             DELIMITED BY ","
             INTO TX-ACTION
                  TX-ACCOUNT-ID
                  TX-AMOUNT
           END-UNSTRING.

       VALIDATE-AND-PROCESS.
           MOVE SPACES TO SQL-COMMAND
           MOVE SPACES TO SQL-LIT
           STRING
              "SELECT balance FROM accounts WHERE account_id = '"
              FUNCTION TRIM(TX-ACCOUNT-ID)
              "'"
              INTO SQL-LIT
           END-STRING
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT))
           MOVE SQL-LIT(1:L) TO SQL-COMMAND(1:L)
           MOVE X"00" TO SQL-COMMAND(L + 1:1)

           CALL "DB_QUERY_SINGLE"
                USING BY VALUE DBH
                      BY REFERENCE SQL-COMMAND
                      BY REFERENCE SINGLE-RESULT-BUFFER
                RETURNING RC
           END-CALL
           
           IF RC NOT = 0
              DISPLAY "ERROR: Could not find account " 
                      FUNCTION TRIM(TX-ACCOUNT-ID)
              EXIT PARAGRAPH
           END-IF

           *> Convert balance
           MOVE FUNCTION TRIM(SINGLE-RESULT-BUFFER) TO WS-BALANCE
           MOVE 0 TO CURRENT-BALANCE
           IF WS-BALANCE NOT = SPACES
               MOVE WS-BALANCE TO CURRENT-BALANCE
               IF CURRENT-BALANCE = 0
                   COMPUTE CURRENT-BALANCE = 
                       FUNCTION NUMVAL(WS-BALANCE)
               END-IF
           END-IF

           *> Convert withdrawal amount
           MOVE FUNCTION TRIM(TX-AMOUNT) TO WS-AMOUNT
           MOVE 0 TO WITHDRAWAL-AMOUNT
           IF WS-AMOUNT NOT = SPACES
               MOVE WS-AMOUNT TO WITHDRAWAL-AMOUNT
               IF WITHDRAWAL-AMOUNT = 0
                   COMPUTE WITHDRAWAL-AMOUNT = 
                       FUNCTION NUMVAL(WS-AMOUNT)
               END-IF
           END-IF

           IF CURRENT-BALANCE >= WITHDRAWAL-AMOUNT
              PERFORM EXECUTE-UPDATE
              IF RC = 0
                 PERFORM LOG-TRANSACTION
              ELSE
                 DISPLAY "ERROR: Update failed for account " 
                         FUNCTION TRIM(TX-ACCOUNT-ID)
              END-IF
           END-IF.

       EXECUTE-UPDATE.
           MOVE SPACES TO SQL-COMMAND
           MOVE SPACES TO SQL-LIT
           STRING
              "UPDATE accounts SET balance = balance - "
              FUNCTION TRIM(TX-AMOUNT)
              " WHERE account_id = '"
              FUNCTION TRIM(TX-ACCOUNT-ID)
              "'"
              INTO SQL-LIT
           END-STRING
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT))
           MOVE SQL-LIT(1:L) TO SQL-COMMAND(1:L)
           MOVE X"00" TO SQL-COMMAND(L + 1:1)

           CALL "DB_EXEC"
                USING BY VALUE DBH
                      BY REFERENCE SQL-COMMAND
                RETURNING RC
           END-CALL.

       LOG-TRANSACTION.
           *> Use direct INSERT into tx_log table instead of stored procedure
           MOVE SPACES TO SQL-COMMAND
           MOVE SPACES TO SQL-LIT
           STRING
              "INSERT INTO tx_log (account_id, tx_type, amount) "
              "VALUES ('"
              FUNCTION TRIM(TX-ACCOUNT-ID)
              "', 'WITHDRAW', "
              FUNCTION TRIM(TX-AMOUNT)
              ")"
              INTO SQL-LIT
           END-STRING
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(SQL-LIT))
           MOVE SQL-LIT(1:L) TO SQL-COMMAND(1:L)
           MOVE X"00" TO SQL-COMMAND(L + 1:1)

           CALL "DB_EXEC"
                USING BY VALUE DBH
                      BY REFERENCE SQL-COMMAND
                RETURNING RC
           END-CALL

           IF RC = 0
              DISPLAY "SUCCESS: Withdrawal and audit log complete for account "
                      FUNCTION TRIM(TX-ACCOUNT-ID)
           ELSE
              DISPLAY "SQL: " FUNCTION TRIM(SQL-COMMAND)
           END-IF.

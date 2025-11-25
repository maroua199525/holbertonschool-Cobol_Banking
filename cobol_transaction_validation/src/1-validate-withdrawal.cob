IDENTIFICATION DIVISION.
       PROGRAM-ID. validate-withdrawal.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TX-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS TX-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  TX-FILE.
       01  TX-RECORD            PIC X(200).
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  CONN-LIT PIC X(200) 
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  L PIC 9(4) VALUE 0.
       01  TX-FILE-STATUS PIC XX.
       01  TX-DATA.
           05 TX-ACTION         PIC X(8).
           05 TX-ACCOUNT-ID     PIC X(4).
           05 TX-AMOUNT         PIC X(10).
       01  CURRENT-BALANCE      PIC S9(8)V99.
       01  WITHDRAWAL-AMOUNT    PIC S9(8)V99.
       01  BALANCE-STR          PIC X(20).
       01  WS-BALANCE           PIC X(20).
       01  WS-AMOUNT            PIC X(10).
       01  I PIC 9(2).
       01  J PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE L = FUNCTION LENGTH(FUNCTION TRIM(CONN-LIT)).
           MOVE CONN-LIT(1:L) TO DB-CONNSTR(1:L).
           MOVE X"00" TO DB-CONNSTR(L + 1:1).

           CALL STATIC "DB_CONNECT" USING DB-CONNSTR RETURNING DBH.
           IF DBH = NULL-PTR THEN 
               DISPLAY "ERROR: Database connection failed"
               STOP RUN
           END-IF.

           OPEN INPUT TX-FILE.
           PERFORM PROCESS-WITHDRAWALS UNTIL TX-FILE-STATUS NOT = "00".
           CLOSE TX-FILE.

           CALL STATIC "DB_DISCONNECT" USING BY VALUE DBH RETURNING RC.
           GOBACK.

       PROCESS-WITHDRAWALS.
           READ TX-FILE AT END MOVE "10" TO TX-FILE-STATUS.
           IF TX-FILE-STATUS = "00" THEN
               UNSTRING TX-RECORD DELIMITED BY ","
                   INTO TX-ACTION, TX-ACCOUNT-ID, TX-AMOUNT
               *> Only process WITHDRAW actions
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(TX-ACTION)) = "WITHDRAW"
                   PERFORM VALIDATE-AND-PROCESS
               END-IF
           END-IF.

       VALIDATE-AND-PROCESS.
           MOVE SPACES TO SQL-COMMAND.
           MOVE SPACES TO SINGLE-RESULT-BUFFER.
           STRING 
               "SELECT balance FROM accounts WHERE account_id = '"
               FUNCTION TRIM(TX-ACCOUNT-ID) 
               "'"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND
           END-STRING.
           MOVE X"00" TO SQL-COMMAND(100:1).

           CALL STATIC "DB_QUERY_SINGLE"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND,
                     BY REFERENCE SINGLE-RESULT-BUFFER
               RETURNING RC.


           IF RC = 0 THEN
               *> Clean and validate the balance string
               MOVE FUNCTION TRIM(SINGLE-RESULT-BUFFER) TO WS-BALANCE
               
               
               *> Manual conversion with validation
               PERFORM CONVERT-BALANCE
               
               *> Convert withdrawal amount
               MOVE FUNCTION TRIM(TX-AMOUNT) TO WS-AMOUNT
               PERFORM CONVERT-AMOUNT

               IF CURRENT-BALANCE >= WITHDRAWAL-AMOUNT THEN
                   PERFORM EXECUTE-UPDATE
               ELSE
                   DISPLAY "Validation FAILED: Insufficient funds for account "
                           FUNCTION TRIM(TX-ACCOUNT-ID)
               END-IF
           ELSE
               DISPLAY "ERROR: Could not find account " 
                       FUNCTION TRIM(TX-ACCOUNT-ID)
           END-IF.

       CONVERT-BALANCE.
           *> Initialize to zero
           MOVE 0 TO CURRENT-BALANCE
           
           *> Check if string contains valid numeric data
           IF WS-BALANCE NOT = SPACES THEN
               *> Try direct numeric move first
               MOVE WS-BALANCE TO CURRENT-BALANCE
               
               *> If that fails, try manual parsing
               IF CURRENT-BALANCE = 0 THEN
                   PERFORM PARSE-BALANCE-MANUALLY
               END-IF
           END-IF.

       PARSE-BALANCE-MANUALLY.
           *> Manual parsing for decimal numbers
           MOVE 0 TO CURRENT-BALANCE
           MOVE 1 TO I
           MOVE 0 TO J
           
           *> Find decimal point
           PERFORM VARYING I FROM 1 BY 1 
             UNTIL I > FUNCTION LENGTH(WS-BALANCE)
                   OR WS-BALANCE(I:1) = '.'
           END-PERFORM
           
           IF I <= FUNCTION LENGTH(WS-BALANCE) THEN
               *> We found a decimal point
               COMPUTE CURRENT-BALANCE = 
                   FUNCTION NUMVAL(WS-BALANCE(1:I - 1)) +
                   (FUNCTION NUMVAL(WS-BALANCE(I + 1:)) / 100)
           ELSE
               *> No decimal point, treat as whole number
               COMPUTE CURRENT-BALANCE = FUNCTION NUMVAL(WS-BALANCE)
           END-IF.

       CONVERT-AMOUNT.
           *> Convert withdrawal amount
           MOVE 0 TO WITHDRAWAL-AMOUNT
           IF WS-AMOUNT NOT = SPACES THEN
               MOVE WS-AMOUNT TO WITHDRAWAL-AMOUNT
               IF WITHDRAWAL-AMOUNT = 0 THEN
                   COMPUTE WITHDRAWAL-AMOUNT = 
                       FUNCTION NUMVAL(WS-AMOUNT)
               END-IF
           END-IF.

       EXECUTE-UPDATE.
           MOVE SPACES TO SQL-COMMAND.
           *> Build UPDATE query with quotes around account_id
           STRING 
               "UPDATE accounts SET balance = balance - "
               FUNCTION TRIM(TX-AMOUNT) 
               " WHERE account_id = '"
               FUNCTION TRIM(TX-ACCOUNT-ID) 
               "'"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND
           END-STRING.
           *> Null-terminate
           MOVE X"00" TO SQL-COMMAND(100:1).

           CALL STATIC "DB_EXEC"
               USING BY VALUE DBH, BY REFERENCE SQL-COMMAND
               RETURNING RC.

           IF RC = 0 THEN
               DISPLAY "Validation PASSED: Withdrawal of " 
                       FUNCTION TRIM(TX-AMOUNT)
                       " from account " FUNCTION TRIM(TX-ACCOUNT-ID) 
                       " successful."
           ELSE
               DISPLAY "ERROR: Update failed for account " 
                       FUNCTION TRIM(TX-ACCOUNT-ID)
           END-IF.

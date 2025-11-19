IDENTIFICATION DIVISION.
       PROGRAM-ID. process-transactions.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD     PIC X(80).
       
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       
       01  DATABASE-CONN        PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       
       01  WS-TRANSACTION-FIELDS.
           05  WS-ACTION            PIC X(10).
           05  WS-FIELD2            PIC X(20).
           05  WS-FIELD3            PIC X(50).
           05  WS-FIELD4            PIC X(20).
           05  WS-FIELD5            PIC X(20).
       
       01  WS-CUSTOMER-ID         PIC 9(5).
       01  WS-CUSTOMER-NAME       PIC X(50).
       01  WS-ACCOUNT-ID          PIC 9(5).
       01  WS-AMOUNT              PIC 9(7)V99.
       01  WS-AMOUNT-DISPLAY      PIC 9(7).99.
       01  WS-TRANSACTION-TYPE    PIC X(10).
       
       01  WS-INSERT-CUSTOMER-SQL PIC X(256).
       01  WS-INSERT-ACCOUNT-SQL  PIC X(256).
       01  WS-UPDATE-SQL          PIC X(256).
       
       01  WS-EOF                 PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM CONNECT-TO-DATABASE
           PERFORM OPEN-TRANSACTION-FILE
           PERFORM PROCESS-TRANSACTIONS UNTIL WS-EOF = 'Y'
           PERFORM CLOSE-TRANSACTION-FILE
           PERFORM DISCONNECT-FROM-DATABASE
           GOBACK.
       
       CONNECT-TO-DATABASE.
           MOVE FUNCTION TRIM(DATABASE-CONN) TO DB-CONNSTR
           
           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH
           
           IF DBH = NULL-PTR THEN 
               DISPLAY "Failed to connect to database"
               STOP RUN
           END-IF.
       
       OPEN-TRANSACTION-FILE.
           OPEN INPUT TRANSACTION-FILE.
       
       PROCESS-TRANSACTIONS.
           READ TRANSACTION-FILE
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   PERFORM PARSE-TRANSACTION-LINE
                   PERFORM PROCESS-SINGLE-TRANSACTION
           END-READ.
       
       PARSE-TRANSACTION-LINE.
           INITIALIZE WS-TRANSACTION-FIELDS
           
           UNSTRING TRANSACTION-RECORD DELIMITED BY ','
               INTO WS-ACTION
                    WS-FIELD2
                    WS-FIELD3
                    WS-FIELD4
                    WS-FIELD5
           END-UNSTRING.
       
       PROCESS-SINGLE-TRANSACTION.
           EVALUATE FUNCTION TRIM(WS-ACTION)
               WHEN 'INSERT'
                   PERFORM PROCESS-INSERT
               WHEN 'UPDATE'
                   PERFORM PROCESS-UPDATE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       
       PROCESS-INSERT.
           MOVE FUNCTION NUMVAL(WS-FIELD2) TO WS-CUSTOMER-ID
           MOVE FUNCTION TRIM(WS-FIELD3) TO WS-CUSTOMER-NAME
           MOVE FUNCTION NUMVAL(WS-FIELD4) TO WS-ACCOUNT-ID
           MOVE FUNCTION NUMVAL(WS-FIELD5) TO WS-AMOUNT
           
           PERFORM INSERT-CUSTOMER
           PERFORM INSERT-ACCOUNT
           
           DISPLAY "Processed INSERT for " FUNCTION TRIM(WS-CUSTOMER-NAME).
       
       PROCESS-UPDATE.
           MOVE FUNCTION NUMVAL(WS-FIELD2) TO WS-ACCOUNT-ID
           MOVE FUNCTION TRIM(WS-FIELD3) TO WS-TRANSACTION-TYPE
           MOVE FUNCTION NUMVAL(WS-FIELD4) TO WS-AMOUNT
           
           EVALUATE WS-TRANSACTION-TYPE
               WHEN 'DEPOSIT'
                   PERFORM UPDATE-DEPOSIT
               WHEN 'WITHDRAW'
                   PERFORM UPDATE-WITHDRAW
           END-EVALUATE.
       
       INSERT-CUSTOMER.
           STRING "INSERT INTO customers (customer_id, name) VALUES ("
                  WS-CUSTOMER-ID
                  ", '"
                  FUNCTION TRIM(WS-CUSTOMER-NAME)
                  "')"
           INTO WS-INSERT-CUSTOMER-SQL
           
           MOVE FUNCTION TRIM(WS-INSERT-CUSTOMER-SQL) TO SQL-COMMAND
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.
       
       INSERT-ACCOUNT.
           MOVE WS-AMOUNT TO WS-AMOUNT-DISPLAY
           
           STRING "INSERT INTO accounts (account_id, customer_id, balance) VALUES ("
                  WS-ACCOUNT-ID
                  ", "
                  WS-CUSTOMER-ID
                  ", "
                  FUNCTION TRIM(WS-AMOUNT-DISPLAY)
                  ")"
           INTO WS-INSERT-ACCOUNT-SQL
           
           MOVE FUNCTION TRIM(WS-INSERT-ACCOUNT-SQL) TO SQL-COMMAND
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.
       
       UPDATE-DEPOSIT.
           MOVE WS-AMOUNT TO WS-AMOUNT-DISPLAY
           
           STRING "UPDATE accounts SET balance = balance + "
                  FUNCTION TRIM(WS-AMOUNT-DISPLAY)
                  " WHERE account_id = "
                  WS-ACCOUNT-ID
           INTO WS-UPDATE-SQL
           
           MOVE FUNCTION TRIM(WS-UPDATE-SQL) TO SQL-COMMAND
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT
           
           DISPLAY "Processed DEPOSIT for account " WS-ACCOUNT-ID.
       
       UPDATE-WITHDRAW.
           MOVE WS-AMOUNT TO WS-AMOUNT-DISPLAY
           
           STRING "UPDATE accounts SET balance = balance - "
                  FUNCTION TRIM(WS-AMOUNT-DISPLAY)
                  " WHERE account_id = "
                  WS-ACCOUNT-ID
           INTO WS-UPDATE-SQL
           
           MOVE FUNCTION TRIM(WS-UPDATE-SQL) TO SQL-COMMAND
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT
           
           DISPLAY "Processed WITHDRAW for account " WS-ACCOUNT-ID.
       
       CLOSE-TRANSACTION-FILE.
           CLOSE TRANSACTION-FILE.
       
       DISCONNECT-FROM-DATABASE.
           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.

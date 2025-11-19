IDENTIFICATION DIVISION.
PROGRAM-ID. transaction-handler.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT TRANSACTION-FILE ASSIGN TO "transactions.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS TRANSACTION-STATUS.
DATA DIVISION.
FILE SECTION.
FD  TRANSACTION-FILE.
01  TRANSACTION-RECORD   PIC X(200).
WORKING-STORAGE SECTION.
COPY "dbapi.cpy".
01  DATABASE-CONNECTION  PIC X(200)
    VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
01  TRANSACTION-STATUS   PIC XX.
01  TRANSACTION-DATA.
    05 ACTION-TYPE       PIC X(8).
    05 FILLER            PIC X.
    05 DATA-FIELD-2      PIC X(10).
    05 FILLER            PIC X.
    05 DATA-FIELD-3      PIC X(10).
    05 FILLER            PIC X.
    05 DATA-FIELD-4      PIC X(10).
    05 FILLER            PIC X.
    05 DATA-FIELD-5      PIC X(10).

PROCEDURE DIVISION.
BEGIN-PROCESSING.
    MOVE FUNCTION TRIM(DATABASE-CONNECTION) TO DB-CONNSTR.

    CALL STATIC "DB_CONNECT" 
        USING DB-CONNSTR 
        RETURNING DBH.
    
    IF DBH = NULL-PTR THEN 
        STOP RUN
    END-IF.

    OPEN INPUT TRANSACTION-FILE.
    PERFORM READ-AND-PROCESS UNTIL TRANSACTION-STATUS NOT = "00".
    CLOSE TRANSACTION-FILE.

    CALL STATIC "DB_DISCONNECT" 
        USING BY VALUE DBH 
        RETURNING RC.
    GOBACK.

READ-AND-PROCESS.
    READ TRANSACTION-FILE AT END MOVE "10" TO TRANSACTION-STATUS.
    IF TRANSACTION-STATUS = "00" THEN
        MOVE SPACES TO TRANSACTION-DATA
        UNSTRING TRANSACTION-RECORD DELIMITED BY ","
            INTO ACTION-TYPE, DATA-FIELD-2, DATA-FIELD-3, 
                 DATA-FIELD-4, DATA-FIELD-5
        END-UNSTRING
        
        EVALUATE FUNCTION UPPER-CASE(FUNCTION TRIM(ACTION-TYPE))
            WHEN "INSERT"   PERFORM EXECUTE-INSERT
            WHEN "UPDATE"   PERFORM EXECUTE-UPDATE
        END-EVALUATE
    END-IF.

EXECUTE-INSERT.
    *> Insert into customers table
    MOVE SPACES TO SQL-COMMAND.
    STRING 
        "INSERT INTO customers (customer_id, name) VALUES ("
        FUNCTION TRIM(DATA-FIELD-2) ", '"
        FUNCTION TRIM(DATA-FIELD-3) "')"
        DELIMITED BY SIZE INTO SQL-COMMAND
    END-STRING.
    MOVE X"00" TO SQL-COMMAND(100:1).

    CALL STATIC "DB_EXEC"
        USING BY VALUE DBH
              BY REFERENCE SQL-COMMAND 
        RETURNING RC.
    
    IF RC <> 0 THEN
        DISPLAY "DB_EXEC failed for customers insert: " RC
        EXIT PARAGRAPH
    END-IF.

    *> Insert into accounts table
    MOVE SPACES TO SQL-COMMAND.
    STRING 
        "INSERT INTO accounts (account_id, customer_id, balance) VALUES ("
        FUNCTION TRIM(DATA-FIELD-4) ", "
        FUNCTION TRIM(DATA-FIELD-2) ", "
        FUNCTION TRIM(DATA-FIELD-5) ")"
        DELIMITED BY SIZE INTO SQL-COMMAND
    END-STRING.
    MOVE X"00" TO SQL-COMMAND(100:1).

    CALL STATIC "DB_EXEC"
        USING BY VALUE DBH
              BY REFERENCE SQL-COMMAND 
        RETURNING RC.
    
    IF RC <> 0 THEN
        DISPLAY "DB_EXEC failed for accounts insert: " RC
    ELSE
        DISPLAY "Processed INSERT for " 
                FUNCTION TRIM(DATA-FIELD-3)
    END-IF.

EXECUTE-UPDATE.
    IF FUNCTION UPPER-CASE(FUNCTION TRIM(DATA-FIELD-3)) = "DEPOSIT"
        *> Update account balance for deposit
        MOVE SPACES TO SQL-COMMAND
        STRING 
            "UPDATE accounts SET balance = balance + "
            FUNCTION TRIM(DATA-FIELD-4)
            " WHERE account_id = "
            FUNCTION TRIM(DATA-FIELD-2)
            DELIMITED BY SIZE INTO SQL-COMMAND
        END-STRING
        MOVE X"00" TO SQL-COMMAND(100:1)

        CALL STATIC "DB_EXEC"
            USING BY VALUE DBH
                  BY REFERENCE SQL-COMMAND 
            RETURNING RC
        
        IF RC <> 0 THEN
            DISPLAY "DB_EXEC failed for deposit: " RC
        ELSE
            DISPLAY "Processed DEPOSIT for account " 
                    FUNCTION TRIM(DATA-FIELD-2)
        END-IF
    ELSE
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(DATA-FIELD-3)) = "WITHDRAW"
            *> Update account balance for withdrawal
            MOVE SPACES TO SQL-COMMAND
            STRING 
                "UPDATE accounts SET balance = balance - "
                FUNCTION TRIM(DATA-FIELD-4)
                " WHERE account_id = "
                FUNCTION TRIM(DATA-FIELD-2)
                DELIMITED BY SIZE INTO SQL-COMMAND
            END-STRING
            MOVE X"00" TO SQL-COMMAND(100:1)

            CALL STATIC "DB_EXEC"
                USING BY VALUE DBH
                      BY REFERENCE SQL-COMMAND 
                RETURNING RC
            
            IF RC <> 0 THEN
                DISPLAY "DB_EXEC failed for withdraw: " RC
            ELSE
                DISPLAY "Processed WITHDRAW for account " 
                        FUNCTION TRIM(DATA-FIELD-2)
            END-IF
        END-IF
    END-IF.

IDENTIFICATION DIVISION.
       PROGRAM-ID. complete-database-demo.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  DB-CONNECTION-STRING PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  VALID-QUERY          PIC X(200)
           VALUE "SELECT account_id, balance FROM accounts ORDER BY account_id".
       01  INVALID-QUERY        PIC X(200) 
           VALUE "SELECT nope FROM accounts".

       PROCEDURE DIVISION.
       BEGIN-EXECUTION.
           DISPLAY "START".
           
           MOVE FUNCTION TRIM(DB-CONNECTION-STRING) TO DB-CONNSTR.

           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.

           MOVE FUNCTION TRIM(VALID-QUERY) TO SQL-COMMAND.
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.
           
           IF STMT NOT = NULL-PTR THEN
               PERFORM PROCESS-RESULT-SET UNTIL RC NOT = 0
           END-IF.

           MOVE FUNCTION TRIM(INVALID-QUERY) TO SQL-COMMAND.
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.
           
           IF STMT = NULL-PTR THEN
                DISPLAY "ERROR: Query failed : '" 
                        FUNCTION TRIM(SQL-COMMAND) 
                        ";'"
           END-IF.

           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.
           DISPLAY "END".
           GOBACK.

       PROCESS-RESULT-SET.
           MOVE SPACES TO C1, C2, C3.
           CALL STATIC "DB_FETCH"
               USING BY VALUE STMT
                     BY REFERENCE C1, C2, C3
               RETURNING RC.
           
           IF RC = 0 THEN
               DISPLAY "-> Account " FUNCTION TRIM(C1)
                       ", balance " FUNCTION TRIM(C2)
           END-IF.

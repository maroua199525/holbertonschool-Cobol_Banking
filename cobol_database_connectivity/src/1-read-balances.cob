IDENTIFICATION DIVISION.
       PROGRAM-ID. account-balance-reader.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  DATABASE-CONNECTION-STRING PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  STRING-LENGTH PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       START-PROGRAM.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE STRING-LENGTH = 
               FUNCTION LENGTH(FUNCTION TRIM(DATABASE-CONNECTION-STRING)).
           MOVE DATABASE-CONNECTION-STRING(1:STRING-LENGTH) 
               TO DB-CONNSTR(1:STRING-LENGTH).
           MOVE X"00" TO DB-CONNSTR(STRING-LENGTH + 1:1).

           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.

           MOVE SPACES TO SQL-COMMAND.
           STRING "SELECT account_id, balance FROM accounts "
                  "ORDER BY account_id"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND.

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.

           IF STMT NOT = NULL-PTR THEN
               PERFORM PROCESS-RECORDS UNTIL RC NOT = 0
           END-IF.

           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.
           GOBACK.

       PROCESS-RECORDS.
           MOVE SPACES TO C1, C2, C3.
           CALL STATIC "DB_FETCH"
               USING BY VALUE STMT
                     BY REFERENCE C1, C2, C3
               RETURNING RC.
           IF RC = 0 THEN
               DISPLAY "-> Account " FUNCTION TRIM(C1)
                       ", balance " FUNCTION TRIM(C2)
           END-IF.

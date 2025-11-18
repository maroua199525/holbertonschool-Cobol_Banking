IDENTIFICATION DIVISION.
       PROGRAM-ID. query-error-test.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  DATABASE-CONN-STRING PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  STR-LEN PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       BEGIN-EXECUTION.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE STR-LEN = 
               FUNCTION LENGTH(FUNCTION TRIM(DATABASE-CONN-STRING)).
           MOVE DATABASE-CONN-STRING(1:STR-LEN) 
               TO DB-CONNSTR(1:STR-LEN).
           MOVE X"00" TO DB-CONNSTR(STR-LEN + 1:1).

           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.

           MOVE SPACES TO SQL-COMMAND.
           STRING "SELECT nope FROM accounts"
               DELIMITED BY SIZE 
               INTO SQL-COMMAND.

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
           GOBACK.

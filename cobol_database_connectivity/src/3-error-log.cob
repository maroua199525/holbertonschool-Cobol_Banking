IDENTIFICATION DIVISION.
       PROGRAM-ID. database-error-logger.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERROR-LOG ASSIGN TO "build/db_errors.log"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ERROR-LOG-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  ERROR-LOG.
       01  ERROR-LOG-ENTRY      PIC X(200).
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  LOG-MESSAGE          PIC X(200).
       01  DATABASE-CONNECTION  PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  QUERY-STRING         PIC X(200) 
           VALUE "SELECT wrong_column FROM accounts;".
       01  STR-LENGTH           PIC 9(4) VALUE 0.
       01  ERROR-LOG-STATUS     PIC XX.

       PROCEDURE DIVISION.
       START-PROGRAM.
           MOVE SPACES TO DB-CONNSTR.
           COMPUTE STR-LENGTH = 
               FUNCTION LENGTH(FUNCTION TRIM(DATABASE-CONNECTION)).
           MOVE DATABASE-CONNECTION(1:STR-LENGTH) 
               TO DB-CONNSTR(1:STR-LENGTH).
           MOVE X"00" TO DB-CONNSTR(STR-LENGTH + 1:1).

           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.

           MOVE SPACES TO SQL-COMMAND.
           COMPUTE STR-LENGTH = 
               FUNCTION LENGTH(FUNCTION TRIM(QUERY-STRING)).
           MOVE QUERY-STRING(1:STR-LENGTH) 
               TO SQL-COMMAND(1:STR-LENGTH).
           MOVE X"00" TO SQL-COMMAND(STR-LENGTH + 1:1).

           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.

           IF STMT = NULL-PTR THEN
               PERFORM LOG-ERROR-MESSAGE
           END-IF.

           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.
           GOBACK.

       LOG-ERROR-MESSAGE.
           OPEN INPUT ERROR-LOG.
           IF ERROR-LOG-STATUS = "35" THEN
               OPEN OUTPUT ERROR-LOG
               CLOSE ERROR-LOG
           ELSE
               CLOSE ERROR-LOG
           END-IF.
           
           STRING "[ERROR] Query failed : '" 
                  FUNCTION TRIM(QUERY-STRING)
                  "'"
               INTO LOG-MESSAGE.
           
           OPEN EXTEND ERROR-LOG.
           MOVE LOG-MESSAGE TO ERROR-LOG-ENTRY.
           WRITE ERROR-LOG-ENTRY.
           CLOSE ERROR-LOG.
           DISPLAY "ERROR Logged to build/db_errors.log".

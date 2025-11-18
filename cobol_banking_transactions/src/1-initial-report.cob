IDENTIFICATION DIVISION.
       PROGRAM-ID. balance-report-generator.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "dbapi.cpy".
       01  DATABASE-CONN        PIC X(200)
           VALUE "host=localhost dbname=schooldb user=postgres password=postgres".
       01  REPORT-QUERY         PIC X(200) VALUE
           "SELECT c.name, a.balance FROM customers c "
           & "JOIN accounts a ON c.customer_id = a.customer_id "
           & "ORDER BY c.customer_id".
       
       PROCEDURE DIVISION.
       START-REPORT.
           MOVE FUNCTION TRIM(DATABASE-CONN) TO DB-CONNSTR.
           
           CALL STATIC "DB_CONNECT" 
               USING DB-CONNSTR 
               RETURNING DBH.
           
           IF DBH = NULL-PTR THEN 
               STOP RUN
           END-IF.
           
           MOVE FUNCTION TRIM(REPORT-QUERY) TO SQL-COMMAND.
           
           CALL STATIC "DB_QUERY"
               USING BY VALUE DBH
                     BY REFERENCE SQL-COMMAND
               RETURNING STMT.
           
           IF STMT NOT = NULL-PTR THEN
               DISPLAY "--- INITIAL BALANCE REPORT ---"
               PERFORM RETRIEVE-RECORDS UNTIL RC NOT = 0
           END-IF.
           
           CALL STATIC "DB_DISCONNECT" 
               USING BY VALUE DBH 
               RETURNING RC.
           GOBACK.
       
       RETRIEVE-RECORDS.
           MOVE SPACES TO C1, C2, C3.
           CALL STATIC "DB_FETCH"
               USING BY VALUE STMT
                     BY REFERENCE C1, C2, C3
               RETURNING RC.
           
           IF RC = 0 THEN
               DISPLAY "Customer: " FUNCTION TRIM(C1)
                       ", Balance: " FUNCTION TRIM(C2)
           END-IF.

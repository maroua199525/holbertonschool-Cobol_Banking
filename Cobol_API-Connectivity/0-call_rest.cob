       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALL-REST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESPONSE-FILE ASSIGN TO "response.json"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RESPONSE-FILE.
       01  RESPONSE-RECORD     PIC X(256).

       WORKING-STORAGE SECTION.
       01 API-URL              PIC X(55)
           VALUE "https://jsonplaceholder.typicode.com/todos/1".
       01 CURL-COMMAND         PIC X(200).
       01 SYSTEM-STATUS        PIC S9(9) BINARY.

       01 WS-FILE-STATUS       PIC X.
           88 EOF-REACHED      VALUE 'Y' FALSE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Calling REST API...".

           STRING "curl -s -o response.json " API-URL
               DELIMITED BY SIZE INTO CURL-COMMAND.

           CALL "SYSTEM" USING CURL-COMMAND
                         RETURNING SYSTEM-STATUS.

           IF SYSTEM-STATUS = 0
               DISPLAY "API call successful. Reading response file..."
               PERFORM READ-RESPONSE-FILE
           ELSE
               DISPLAY "Error: API call command failed with status: "
                       SYSTEM-STATUS
           END-IF.

           DISPLAY "Program finished.".
           STOP RUN.

       READ-RESPONSE-FILE.
           SET EOF-REACHED TO FALSE.
           OPEN INPUT RESPONSE-FILE.
           PERFORM UNTIL EOF-REACHED
               READ RESPONSE-FILE
                   AT END
                       SET EOF-REACHED TO TRUE
                   NOT AT END
                       DISPLAY FUNCTION TRIM(RESPONSE-RECORD)
               END-READ
           END-PERFORM.
           CLOSE RESPONSE-FILE.

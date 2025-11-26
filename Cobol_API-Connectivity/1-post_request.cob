       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTREQUEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RespFile ASSIGN TO "post_response.json"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT StatusFile ASSIGN TO "post_status.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RespFile.
       01  RespRecord          PIC X(200).
       FD  StatusFile.
       01  StatusRecord        PIC X(10).

       WORKING-STORAGE SECTION.
       01  Curl-Command        PIC X(500).
       01  API-URL             PIC X(50)
           VALUE "https://jsonplaceholder.typicode.com/posts".
       01  JSON-PAYLOAD        PIC X(120) VALUE "{"
           & " ""title"":""A COBOL Post"","
           & 
           " ""body"":""This is a post created from a COBOL program."","
           & " ""userId"":1"
           & "}".
       01  Id-Value-Num        PIC 9(10).
       01  Id-Value-Display    PIC Z(9)9.
       01  SYSTEM-STATUS       PIC S9(9) BINARY.
       01  Response-String     PIC X(1024).
       01  Remainder-String    PIC X(200).
       01  Temp-Value          PIC X(200).
       01  HTTP-Status-Code    PIC 9(3).
       01  WS-File-Status      PIC X.
           88 EOF-Reached      VALUE 'Y' FALSE 'N'.
       01  WS-Pointer          PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Making POST request to JSONPlaceholder...".

           STRING "curl -s -X POST " API-URL
                  " -H ""Content-Type: application/json"" "
                  "-d '" JSON-PAYLOAD "' "
                  "-o post_response.json "
                  "-w '%{http_code}' > post_status.txt"
                  DELIMITED BY SIZE INTO Curl-Command.

           CALL "SYSTEM" USING Curl-Command
                         RETURNING SYSTEM-STATUS.

           IF SYSTEM-STATUS = 0
               PERFORM READ-STATUS-FILE
               IF HTTP-Status-Code = 201
                   DISPLAY "API call successful. HTTP Status: "
                           HTTP-Status-Code
                   PERFORM READ-ENTIRE-RESPONSE-FILE
                   PERFORM PARSE-JSON-RESPONSE
                   MOVE Id-Value-Num TO Id-Value-Display
                   DISPLAY "New post ID: " FUNCTION 
                   TRIM(Id-Value-Display)
               ELSE
                   DISPLAY "API call failed. HTTP Status: "
                           HTTP-Status-Code
               END-IF
           ELSE
               DISPLAY "Error executing curl command. Status: "
                       SYSTEM-STATUS
           END-IF.

           DISPLAY "Done.".
           STOP RUN.

       READ-STATUS-FILE.
           OPEN INPUT StatusFile.
           READ StatusFile INTO StatusRecord.
           CLOSE StatusFile.
           MOVE StatusRecord TO HTTP-Status-Code.

       READ-ENTIRE-RESPONSE-FILE.
           INITIALIZE Response-String.
           MOVE 1 TO WS-Pointer.
           SET EOF-Reached TO FALSE.
           OPEN INPUT RespFile.
           PERFORM UNTIL EOF-Reached
               READ RespFile
                   AT END
                       SET EOF-Reached TO TRUE
                   NOT AT END
                       STRING FUNCTION TRIM(RespRecord)
                           DELIMITED BY SIZE
                           INTO Response-String
                           WITH POINTER WS-Pointer
                       END-STRING
               END-READ
           END-PERFORM.
           CLOSE RespFile.

       PARSE-JSON-RESPONSE.
           UNSTRING Response-String DELIMITED BY ',"id": '
               INTO Temp-Value
                    Remainder-String
           END-UNSTRING.

           UNSTRING Remainder-String DELIMITED BY '}'
               INTO Id-Value-Num
           END-UNSTRING.

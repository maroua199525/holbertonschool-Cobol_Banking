       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATE-POST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO "temp_data.json".
           SELECT RESP-FILE ASSIGN TO "updated_response.json".
           SELECT STATUS-FILE ASSIGN TO "status.txt".

       DATA DIVISION.
       FILE SECTION.
       FD DATA-FILE.
       01 DATA-RECORD      PIC X(200).
       FD RESP-FILE.
       01 RESP-RECORD      PIC X(500).
       FD STATUS-FILE.
       01 STATUS-RECORD    PIC X(10).

       WORKING-STORAGE SECTION.
       01 API-URL          PIC X(50) VALUE 
       "https://jsonplaceholder.typicode.com/posts/1".
       01 JSON-PAYLOAD     PIC X(200) VALUE 
           "{"
           & " ""id"": 1,"
           & " ""title"": ""A COBOL Updated Title"","
           & " ""body"": ""This post was updated from a COBOL program.""
      -    ","
           & " ""userId"": 1 "
           & "}".
       01 CURL-COMMAND     PIC X(500).
       01 SYSTEM-STATUS    PIC S9(9) BINARY.
       01 HTTP-STATUS-CODE PIC 9(3).
       01 RESPONSE-BODY    PIC X(500).
       01 Parsed-ID        PIC 9(10).
       01 Temp-String      PIC X(500).

       PROCEDURE DIVISION.
           PERFORM WRITE-JSON-FILE.
           STRING "curl -s -X PUT "
                  "-H ""Content-Type: application/json"" "
                  "-d @" "temp_data.json"
                  " " API-URL
                  " -o " "updated_response.json"
                  " -w '%{http_code}' > " "status.txt"
                  DELIMITED BY SIZE INTO CURL-COMMAND.
           DISPLAY "Executing command: " CURL-COMMAND.
           CALL "SYSTEM" USING CURL-COMMAND
                         RETURNING SYSTEM-STATUS.
           IF SYSTEM-STATUS = 0
               DISPLAY "Curl command executed successfully. Checking res
      -         "ponse..."
               PERFORM READ-STATUS-CODE
               IF HTTP-STATUS-CODE >= 200 AND HTTP-STATUS-CODE < 300
                   DISPLAY "PUT request successful! HTTP Status: " 
                   HTTP-STATUS-CODE
                   PERFORM READ-RESPONSE-BODY
                   PERFORM PARSE-JSON-RESPONSE
                   DISPLAY "Successfully updated post with ID: " 
                   Parsed-ID
               ELSE
                   DISPLAY "PUT request failed. HTTP Status: " 
                   HTTP-STATUS-CODE
               END-IF
           ELSE
               DISPLAY "Error executing curl command. System Status: " 
               SYSTEM-STATUS
           END-IF.
           STOP RUN.

       WRITE-JSON-FILE.
           OPEN OUTPUT DATA-FILE.
           WRITE DATA-RECORD FROM JSON-PAYLOAD.
           CLOSE DATA-FILE.

       READ-STATUS-CODE.
           OPEN INPUT STATUS-FILE.
           READ STATUS-FILE INTO STATUS-RECORD.
           MOVE STATUS-RECORD TO HTTP-STATUS-CODE.
           CLOSE STATUS-FILE.

       READ-RESPONSE-BODY.
           OPEN INPUT RESP-FILE.
           READ RESP-FILE INTO RESP-RECORD.
           MOVE RESP-RECORD TO RESPONSE-BODY.
           CLOSE RESP-FILE.

       PARSE-JSON-RESPONSE.
           UNSTRING RESPONSE-BODY DELIMITED BY '"id": '
               INTO Temp-String
                    RESPONSE-BODY
           END-UNSTRING.

           UNSTRING RESPONSE-BODY DELIMITED BY ','
               INTO Parsed-ID
           END-UNSTRING.
           
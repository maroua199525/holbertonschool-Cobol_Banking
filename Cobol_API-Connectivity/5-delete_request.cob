       IDENTIFICATION DIVISION.
       PROGRAM-ID. ParseJsonArrayDual.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TodosFile ASSIGN TO "todos.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TodosFile.
       01  Todo-Line        PIC X(200).

       WORKING-STORAGE SECTION.
       77  WS-Command       PIC X(300).
       77  WS-Status        PIC S9(9) COMP-5.
       77  WS-EOF           PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "Fetching raw JSON and extracting to-do titles...".

      * Step 1: Save raw JSON to file
           MOVE "curl -s https://jsonplaceholder.typicode.com/todos?_lim
      -    "it=10 > todos.json"
             TO WS-Command.
           CALL "SYSTEM" USING WS-Command RETURNING WS-Status.

           IF WS-Status NOT = 0
              DISPLAY 
              "Error: Failed to fetch JSON (status " WS-Status ")"
              STOP RUN
           END-IF.

      * Step 2: Extract only titles with jq into todos.txt
           MOVE "jq -r '.[].title' todos.json > todos.txt"
             TO WS-Command.
           CALL "SYSTEM" USING WS-Command RETURNING WS-Status.

           IF WS-Status NOT = 0
              DISPLAY "Error: jq failed (status " WS-Status ")"
              STOP RUN
           END-IF.

      * Step 3: Read and display titles
           DISPLAY "API call successful. Displaying titles:".
           OPEN INPUT TodosFile
           PERFORM UNTIL WS-EOF = "Y"
               READ TodosFile
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END DISPLAY FUNCTION TRIM (Todo-Line)
               END-READ
           END-PERFORM
           CLOSE TodosFile.

           DISPLAY "Done.".
           STOP RUN.
           
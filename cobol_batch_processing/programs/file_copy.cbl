 IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECOPY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "datasets/input_data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "datasets/output_data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           DATA RECORD IS IN-REC.
       01 IN-REC PIC X(80).

       FD  OUTFILE
           DATA RECORD IS OUT-REC.
       01 OUT-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-COUNT    PIC 9(5) VALUE 0.
       01 EOF-INFILE  PIC X VALUE 'N'.  *> 'N' = FALSE, 'Y' = TRUE

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "FILE-COPY: Starting file processing..."
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE

           PERFORM UNTIL EOF-INFILE = 'Y'
               READ INFILE
                   AT END
                       MOVE 'Y' TO EOF-INFILE
                   NOT AT END
                       MOVE IN-REC TO OUT-REC
                       WRITE OUT-REC
                       ADD 1 TO WS-COUNT
               END-READ
           END-PERFORM

           CLOSE INFILE OUTFILE
           DISPLAY "FILE-COPY: Processing completed"
           DISPLAY "FILE-COPY: Records processed: " WS-COUNT
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILECOPY.
       AUTHOR. STUDENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "INFILE"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO "OUTFILE"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  INPUT-RECORD            PIC X(80).
       
       FD  OUTFILE.
       01  OUTPUT-RECORD           PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-RECORD-COUNT         PIC 9(5) VALUE 0.
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "FILE-COPY: Starting file processing..."
           
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           
           PERFORM READ-AND-COPY UNTIL WS-EOF-FLAG = 'Y'
           
           CLOSE INFILE
           CLOSE OUTFILE
           
           DISPLAY "FILE-COPY: Processing completed"
           DISPLAY "FILE-COPY: Records processed: " WS-RECORD-COUNT
           
           STOP RUN.
       
       READ-AND-COPY.
           READ INFILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-RECORD-COUNT
                   MOVE INPUT-RECORD TO OUTPUT-RECORD
                   WRITE OUTPUT-RECORD
           END-READ.

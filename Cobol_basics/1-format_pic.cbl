       IDENTIFICATION DIVISION.
       PROGRAM-ID. FormatPicExample.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Customer-ID          PIC X(9)        VALUE "CUST00123".
       01 Account-Balance      PIC 9(5)V99     VALUE 1234.56.
       01 Interest-Rate        PIC 9(1)V99     VALUE 5.75.

       PROCEDURE DIVISION.

       DISPLAY "Customer ID : " Customer-ID.
       DISPLAY "Account Balance : " Account-Balance.
       DISPLAY "Interest Rate : " Interest-Rate "%" .
       
       STOP RUN.

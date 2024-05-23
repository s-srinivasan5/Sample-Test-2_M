       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-STUDENT-ID PIC 9(4) VALUE 1001.
       01 WS-STUDENT-NAME PIC A(15) VALUE 'Tim'.
       01 WS-EXTRA        PIC X(10) VALUE "DUMMY".
      *
       PROCEDURE DIVISION.
           
           CALL 'UTIL' USING WS-STUDENT-ID, WS-STUDENT-NAME, WS-EXTRA.
           
           DISPLAY 'Student Id : ' WS-STUDENT-ID
           DISPLAY 'Student Name : ' WS-STUDENT-NAME
           DISPLAY 'WS-EXTRA : ' WS-EXTRA
           
           STOP RUN.

       END PROGRAM MAIN.
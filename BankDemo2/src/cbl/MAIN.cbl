       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01   WS-STUDENT-ID PIC 9(4) VALUE 1000.
       01   WS-STUDENT-NAME PIC A(15) VALUE 'Tim'.
       01   WS-EXTRA-1       PIC X(10) value low-values.
      *
       PROCEDURE DIVISION.
           
           CALL 'UTIL2' USING WS-STUDENT-ID, 
                             WS-STUDENT-NAME.
           
           DISPLAY 'Student Id : ' WS-STUDENT-ID
           DISPLAY 'Student Name : ' WS-STUDENT-NAME
           
           STOP RUN.

       END PROGRAM MAIN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTIL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ws-dummy         USAGE POINTER VALUE NULL.                    
       01 WS-EXTRA-1       PIC X(10) value low-values.

       LINKAGE SECTION.
       
       01 LS-STUDENT-ID PIC 9(4).
       01 LS-STUDENT-NAME PIC A(15).
       01 LS-EXTRA-1   PIC X(10) value low-values.

       PROCEDURE DIVISION USING LS-STUDENT-ID, 
                                LS-STUDENT-NAME.

           set address of LS-EXTRA-1 to null.
           move LS-EXTRA-1 to WS-EXTRA-1.
                      
           DISPLAY 'In Called Program'.

      *    set address of LS-EXTRA-1 to ws-dummy.
           
           
      *    IF  LS-EXTRA-1 = " "
      *        DISPLAY "LS-EXTRA-1 : " LS-EXTRA-1
              
      *    END-IF
           
      *    CALL "PGMC" USING VARA
      *    
      *    IF VARA=TRUE 
      *        PERFORM PARAA
           
           IF LS-STUDENT-ID = 1001
              MOVE 'CALLED DUMMY' TO LS-EXTRA-1 
              MOVE 1221 TO LS-STUDENT-ID
              MOVE 'JHON' TO LS-STUDENT-NAME
           ELSE
              MOVE 1111 TO LS-STUDENT-ID
              MOVE 'ADAM' TO LS-STUDENT-NAME
              MOVE 'MAIN-1' TO LS-EXTRA-1
           END-IF
           
           
           
           
           EXIT PROGRAM.

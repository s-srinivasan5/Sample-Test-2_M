       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOBL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       special-names.
           call-convention 74 is winapi.  
       
       DATA DIVISION.
     
       WORKING-STORAGE SECTION.

       01  pp procedure-pointer.
       01 system-time.
         03 system-year     pic 9(4) comp-5.
         03 system-month    pic 9(4) comp-5.
         03 system-day-of-week  pic 9(4) comp-5.
         03 system-day    pic 9(4) comp-5.
         03 system-hour    pic 9(4) comp-5.
         03 system-minute   pic 9(4) comp-5.
         03 system-second   pic 9(4) comp-5.
         03 system-millisecond  pic 9(4) comp-5.

       PROCEDURE DIVISION.

           set pp to entry "kernel32"
           call winapi "GetSystemTime" using
                  by reference system-time
           display "Day of week is:  " system-day-of-week upon console
           display "Day of month is:  " system-day upon console

           GOBACK.

       END PROGRAM TESTCOBL.

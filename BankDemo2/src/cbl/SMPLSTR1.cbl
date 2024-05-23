       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMPLSTR1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  BINARY-WORK-AREAS                                COMP.       
               10  WS-CICSRESP                     PIC S9(8).           
                 88  CICS-RESP-NORMAL   VALUE DFHRESP(NORMAL).          
                 88  CICS-RESP-QIDERR   VALUE DFHRESP(QIDERR).          
                 88  CICS-RESP-SYSIDERR VALUE DFHRESP(SYSIDERR).        
               10  WS-CICSRESP2                    PIC S9(8).           
                                                                 
001362 01  ws-start-misc.
001364     05 start-resp                     pic s9(08) comp.
001366     05 start-resp2                    pic s9(08) comp.
001368     05 start-from                     pic x(30).
001370     05 start-length                   pic s9(04) comp.
001372     05 start-transid                  pic x(04).
001374     05 start-userid                   pic x(08).
001376     05 start-resp-disp                pic +9(08).
001378     05 start-resp2-disp               pic +9(08).

       PROCEDURE DIVISION.

007394*    exec cics start
007396*      resp     ( start-resp           of ws-start-misc )
007398*      resp2    ( start-resp2          of ws-start-misc )
007400*      transid  ( start-transid        of ws-start-misc )
007408*      nohandle
007410*    end-exec
       
                  EXEC CICS LINK 
                   PROGRAM("caszcesn")
                   TRANSID("CESN")
                   APPLID("BANKDEMO")
                   RESP(WS-CICSRESP) RESP2(WS-CICSRESP2)
                  END-EXEC.

      *            EXEC CICS START 
      *                TRANSID("CESN")
      *                RESP(WS-CICSRESP) RESP2(WS-CICSRESP2)
      *            END-EXEC
                   
                   DISPLAY "WS-CICSRESP : " WS-CICSRESP
                   DISPLAY "WS-CICSRESP2 : " WS-CICSRESP2
                  
                   
                   
                  EXEC CICS LINK 
                   PROGRAM("dfhzcmap")
                   TRANSID("CMAP")
                   APPLID("BANKDEMO")
                   RESP(WS-CICSRESP) RESP2(WS-CICSRESP2)
                  END-EXEC.
 
                   DISPLAY "WS-CICSRESP : " WS-CICSRESP
                   DISPLAY "WS-CICSRESP2 : " WS-CICSRESP2                   
                   
           GOBACK.

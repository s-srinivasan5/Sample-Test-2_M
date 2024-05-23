       IDENTIFICATION DIVISION.                                         00020000
       PROGRAM-ID.  SMPLSTRT.                                           00030000

           EJECT                                                        00690000
       DATA DIVISION.                                                   00700000
       WORKING-STORAGE SECTION.                                         00710000
                                                                        00720000
      ***************************************************************** 01490000
      *W                                                              * 01500000
      *            V A R I A B L E   W O R K   A R E A S              * 01510000
      *                                                               * 01520000
      ***************************************************************** 01530000
                                                                        01540000
       01  VARIABLE-WORK-AREAS.                                         01550000
                                                                        01560000
                                                                        01680000
           05  BINARY-WORK-AREAS                                COMP.   01690000
               10  WS-CICSRESP                     PIC S9(8).           01700000
                 88  CICS-RESP-NORMAL   VALUE DFHRESP(NORMAL).          01710000
                 88  CICS-RESP-QIDERR   VALUE DFHRESP(QIDERR).          01720000
                 88  CICS-RESP-SYSIDERR VALUE DFHRESP(SYSIDERR).        01730000
               10  WS-CICSRESP2                    PIC S9(8).           01740000
                                                                        01840000
                                                                        01950000
           05  MISCELLANEOUS-WORK-AREAS.                                01960000
DEBUG          10  WS-CKVSMLOG-FPR                 FUNCTION-POINTER.    01970001
DEBUG          10  WS-CKVSMLOG-PTR REDEFINES                            01980001
DEBUG              WS-CKVSMLOG-FPR                 POINTER.             01990001
               10  WS-REQID                        PIC X(8).            02000001
               10  WS-NEWTRAN                      PIC X(4).            02010000
                 88  NEWTRAN-0005                VALUE '0005'.          02020000
                 88  NEWTRAN-0009                VALUE '0009'.          02030000
               10  WS-QUEUEID.                                          02040000
                   15  WS-QUEUEPFX                 PIC X(4).            02050000
                   15  WS-QUEUETRN                 PIC X(4).            02060000
               10  WS-EXTERNAL-DATE.                                    02070000
                   15  WS-EXT-MM                   PIC X(2).            02080000
                   15  WS-EXT-DD                   PIC X(2).            02090000
                   15  WS-EXT-YYYY.                                     02100000
                       20  WS-EXT-CC               PIC X(2).            02110000
                       20  WS-EXT-YY               PIC X(2).            02120000
               10  WS-MESSAGE-DATE                 PIC X(8).            02130000
               10  WS-FORMATTED-DATE.                                   02140000
                   15  WS-FMT-MM                   PIC X(2).            02150000
                   15  FILLER                      PIC X(1).            02160000
                   15  WS-FMT-DD                   PIC X(2).            02170000
                   15  FILLER                      PIC X(1).            02180000
                   15  WS-FMT-YYYY.                                     02190000
                       20  WS-FMT-YY               PIC X(2).            02200000
                       20  FILLER                  PIC X(2).            02210000
               10  WS-HWORD                        PIC S9(4)    COMP.   02220000
               10  FILLER REDEFINES WS-HWORD.                           02230000
                   15  FILLER                      PIC X(1).            02240000
                   15  WS-BINARY-LOW               PIC X(1).            02250000
CK1373         10  WS-CYVCON-ENTRY-NAME              PIC X(8).          02260000
CK1373           88  ENTRY-NAME-FILTABL            VALUE 'FILTABL'.     02270000
               10  WS-CTWAADDR                                  POINTER.02280000
               10  FILLER REDEFINES WS-CTWAADDR    PIC X(4).            02290000
                 88  NULL-TWA-ADDR                 VALUE X'FF000000'.   02300000
CK0850         10  WS-CONSOLE-MESSAGE              PIC X(256).          02310000
               10  WS-CICS-ERR-COMMENT             PIC X(80).           02320000
               10  WS-CICS-ERR-MSG                 PIC X(80).           02330000
               10  WS-RMTTRAN                      PIC X(80).           02340000
               10  WS-RMSYSTBL                     PIC X(100).          02350000
               10  WS-IO-KEY                       PIC X(80).           02360000
               10  WS-MY-SYSID                     PIC X(4).            02370000
               10  WS-APPLID                       PIC X(8).            02380000
               10  WS-TIME-FORMATTED               PIC X(8).            02390000
               10  WS-TIME-DISPLAY                 PIC 9(15).           02400000
               10  FILLER REDEFINES WS-TIME-DISPLAY.                    02410000
                   15  WS-TIME-HH                  PIC 9(2).            02420000
                   15  WS-TIME-MM                  PIC 9(2).            02430000
                   15  WS-TIME-SS                  PIC 9(2).            02440000
               10  WS-FORCEDTE                     PIC X(1).            02450000
                 88  FORCEDT-NOT-FORCED          VALUE '0'.             02460000
                 88  FORCEDT-FROM-MASTER-TRM     VALUE '1'.             02470000
                 88  FORCEDT-FROM-INFORCE        VALUE '2'.             02480000
                 88  FORCEDT-DATE-FORCED         VALUE '1' '2'.         02490000
               10  WS-NEWSTATE                     PIC X(1).            02500000
                 88  NEWSTATE-SSSYSUP              VALUE '1'.           02510000
                 88  NEWSTATE-SSSYSBT              VALUE '2'.           02520000
               10  WS-IO-REQUEST-CODE              PIC X(1).            02530000
                 88  IO-REQUEST-OPEN-ALL-FILES     VALUE '4'.           02540000
                 88  IO-REQUEST-OPEN-ALL-READONLY  VALUE 'X'.           02550000
                 88  IO-REQUEST-CLOSE-AND-REOPEN   VALUE 'A'.           02560000
                 88  IO-REQUEST-CLOSE-ALL-FILES    VALUE '5'.           02570000
                 88  IO-REQUEST-DELETE             VALUE 'D'.           02580000
CK1466           88  IO-REQUEST-DELETE-TSQ         VALUE 'D'.           02590000
                 88  IO-REQUEST-ENQ                VALUE 'E'.           02600000
CK1466           88  IO-REQUEST-FREEMAIN           VALUE 'F'.           02610000
                 88  IO-REQUEST-FLIP-DPFS          VALUE '7'.           02620000
                 88  IO-REQUEST-READ-UPDATE        VALUE 'R'.           02630000
                 88  IO-REQUEST-UPDATE             VALUE 'U'.           02640000
                 88  IO-REQUEST-UNLOCK             VALUE 'F'.           02650000
                 88  IO-REQUEST-READ-TSQ           VALUE '0'.           02660000
                 88  IO-REQUEST-WRITE-NEW          VALUE '1'.           02670000
                 88  IO-REQUEST-WRITE-SYSSTAT      VALUE '1'.           02680000
                 88  IO-REQUEST-REWRITE-SYSSTAT    VALUE '3'.           02690000
                 88  IO-REQUEST-READ-SYSSTAT       VALUE '8'.           02700000
               10  WS-IO-RETURN-CODE               PIC X(1).            02710000
                 88  IO-RETURN-NORMAL              VALUE '0'.           02720000
               10  WS-IO-FILENAME                  PIC X(8).            02730000
                 88  FILENAME-DPFS                 VALUE 'FUDDPFS'.     02740000
                 88  FILENAME-ENQDEQ               VALUE 'ENQDEQ'.      02750000
                 88  FILENAME-TMPSTRG              VALUE 'TMPSTRG'.     02760000
CK1466           88  FILENAME-MEMSHR               VALUE 'MEMSHR'.      02770000
               10  WS-FCSTAT                       PIC X(1).            02780000
                 88  FCSTAT-NORMAL               VALUE '0'.             02790000
               10  WS-OPERATOR                     PIC X(8).            02800000
               10  WS-OPTYPE                       PIC X(1).            02810000
                 88  OPTYPE-PRIMARY                VALUE 'P'.           02820000
                 88  OPTYPE-SECONDARY              VALUE 'S'.           02830000
               10  WS-ENVIRONMENT-IND              PIC X(1).            02840000
                 88  ENVIRONMENT-IS-OFFLINE        VALUE '0'.           02850000
                 88  ENVIRONMENT-IS-ONLINE         VALUE '1' '2'.       02860000
                 88  ENVIRONMENT-IS-CK-CICS        VALUE '1'.           02870000
                 88  ENVIRONMENT-IS-BATCH-SIM      VALUE '2'.           02880000
                 88  ENVIRONMENT-IS-NON-CK-CICS    VALUE '3'.           02890000
               10  WS-UPSI-EXPANDED.                                    02900000
                   15  FILLER                      PIC X(1).            02910000
                   15  FILLER                      PIC X(1).            02920000
                   15  FILLER                      PIC X(1).            02930000
                   15  FILLER                      PIC X(1).            02940000
                   15  FILLER                      PIC X(1).            02950000
                   15  FILLER                      PIC X(1).            02960000
                   15  FILLER                      PIC X(1).            02970000
                     88  UPSI-TP-OR-REPLAY         VALUE '1'.           02980000
                   15  FILLER                      PIC X(1).            02990000
               10  WS-LOG-IND                      PIC X(1).            03000000
                                                                        03010000
       01  WS-CKCOMRG                              PIC X(300).          03020000
                                                                        03030000
       01  WS-SYS-FILE-RECORD.                                          03040000
           05  WS-FSTAT-KEY                        PIC X(3).            03050000
             88  FILSTAT-TYPE-5-KEY                VALUE X'000005'.     03060000
           05  WS-FSTAT-STATUS-AREA.                                    03070000
               10  WS-FSTAT-STATUS                 PIC X(1)             03080000
                 OCCURS 256 TIMES.                                      03090000
           EJECT                                                        03100000

           EJECT                                                        03330000
       LINKAGE SECTION.                                                 03340000
                                                                        03350000
           COPY DFHEIBLK.                                               04090000
           EJECT                                                        04100000
       PROCEDURE DIVISION.
                                                                        04150000
      *            EXEC CICS START                                      14940000
      *              TRANSID('0001') INTERVAL(0)
      *              RESP(WS-CICSRESP) RESP2(WS-CICSRESP2)              14970000
      *            END-EXEC.
      *
      *            EXEC CIC-START TRANSID("0001")
      *            END-EXEC
      *
                  EXEC CICS
                   LINK PROGRAM("CKAI0028")
                   TRANSID("0001")
      *            COMMAREA(WS-AREA)
      *            LENGTH(LENGTH OF WS-AREA)
                   RESP(WS-CICSRESP) RESP2(WS-CICSRESP2)
                  END-EXEC.                   
                   
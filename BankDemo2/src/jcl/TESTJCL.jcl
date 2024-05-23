//SPLCALL JOB 'SPLCALL',CLASS=A,MSGCLASS=X,TIME=1440,
//        COND=(8,LT),NOTIFY=MYUSERID,RESTART=*,LINES=(500,CANCEL)
//*
//* 
//STEP01 EXEC PGM=SMPLSTR1
//SYSOUT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//
//*
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=* 
//SYSUT1   DD DSN=CKBASE.TCCBLBAS.ADVANCE.EXTFMB,DISP=SHR              
//SYSUT2   DD DSN=CKBASE.TCCBLBAS.ADVANCE.EXTFMB.FB,                   
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),
//            DCB=(DSORG=PS,RECFM=FB,LRECL=27990)
//* 
//STEP2    EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=* 
//SYSUT1   DD DSN=CKBASE.TCBCHBAS.ADVANCE.EXTFMB,DISP=SHR
//SYSUT2   DD DSN=CKBASE.TCBCHBAS.ADVANCE.EXTFMB.FB,                   
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),
//            DCB=(DSORG=PS,RECFM=FB,LRECL=27998)
//*
//* 
//STEP3    EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=* 
//SYSUT1   DD DSN=CKBASE.TCCBLBAS.ADVANCE.EXTFMB,DISP=SHR              
//SYSUT2   DD DSN=CKBASE.TCCBLBAS.ADVANCE.EXTFMB.LSEQDF,               
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),
//            DCB=(DSORG=PS,RECFM=LSEQ,LRECL=27990)
//* 
//STEP4    EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=* 
//SYSUT1   DD DSN=CKBASE.TCBCHBAS.ADVANCE.EXTFMB,DISP=SHR
//SYSUT2   DD DSN=CKBASE.TCBCHBAS.ADVANCE.EXTFMB.LSEQDF,               
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),
//            DCB=(DSORG=PS,RECFM=LSEQ,LRECL=27998)
//            
//* 
//STEP1    EXEC PGM=IEBCOMPR
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  DSN=CKBASE.TCBCHBAS.ADVANCE.EXTFMB,DISP=SHR 
//SYSUT2   DD  DSN=CKBASE.TCCBLBAS.ADVANCE.EXTFMB,DISP=SHR
//SYSIN    DD  *
      COMPARE  TYPORG=PO
/*
//
//* 
//STEP1 EXEC PGM=MAIN
//SYSOUT DD *
//SYSPRINT DD  SYSOUT=* 
//
//* 
//STEP1 EXEC PGM=TESTCOBL
//SYSOUT DD *
//SYSPRINT DD  SYSOUT=* 
//*
//*STEP2 EXEC PGM=MAIN2
//* 
//* 
//GDGSTEP3   EXEC PGM=IEFBR14
//DD01     DD DSN=CKBASE.SC.SPCPRJ.UTABLES.FMB.BKPAS2.KAV.VRCG,        
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),
//            DCB=(DSORG=PS,RECFM=VB,LRECL=27998)
//            
//* 
//DELETDUP EXEC PGM=SYNCTOOL                                     
//TOOLMSG  DD  SYSOUT=*                                          
//DFSMSG   DD  SYSOUT=*                                          
//SYSOUT   DD  SYSOUT=*              
//SYSPRINT DD  SYSOUT=*
//SORTWK01 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTWK02 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTWK03 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTIN   DD  DSN=CKBASE.EXUBBAPM.POLMAST.CLTOBO.EXTMV,         
//             DISP=SHR,                                         
//             DCB=BUFNO=20                                      
//CTL1OFT1 DD DSN=CKBASE.EXUBBAPM.PLDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT2 DD DSN=CKBASE.EXUBBAPM.CDDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT3 DD DSN=CKBASE.EXUBBAPM.MIDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT4 DD DSN=CKBASE.EXUBBAPM.MODIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT5 DD DSN=CKBASE.EXUBBAPM.FADIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT6 DD DSN=CKBASE.EXUBBAPM.LSDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL2OFT7 DD DSN=CKBASE.EXUBBAPM.PTDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//TOOLIN    DD   *                                               
  SORT FROM(SORTIN) USING(CTL1)                                    
  SORT FROM(SORTIN) USING(CTL2)                                    
//CTL1CNTL  DD *                                                 
  OUTFIL FNAMES=CTL1OFT1,INCLUDE=(5,2,CH,EQ,C'PL')                     
  OUTFIL FNAMES=CTL1OFT2,INCLUDE=(5,2,CH,EQ,C'CV')                     
  OUTFIL FNAMES=CTL1OFT3,INCLUDE=(5,2,CH,EQ,C'FA')                     
  OUTFIL FNAMES=CTL1OFT4,INCLUDE=(5,2,CH,EQ,C'MI')                     
  OUTFIL FNAMES=CTL1OFT5,INCLUDE=(5,2,CH,EQ,C'MO')                     
  OUTFIL FNAMES=CTL1OFT6,INCLUDE=(5,2,CH,EQ,C'PT')                     
  SORT FIELDS=(5,396,BI,A)                                         
  SUM FIELDS=NONE                                                  
//CTL2CNTL  DD *                                                 
  OUTFIL FNAMES=CTL2OFT7,INCLUDE=(5,2,CH,EQ,C'LS')                     
  SORT FIELDS=(5,40,BI,A,48,54,BI,A)                               
  SUM FIELDS=NONE                                                  
/*                                                               
// 
//* 
//DELETDUP EXEC PGM=SYNCTOOL                                     
//TOOLMSG  DD  SYSOUT=*                                          
//DFSMSG   DD  SYSOUT=*                                          
//SYSOUT   DD  SYSOUT=*              
//SYSPRINT DD  SYSOUT=*
//SORTWK01 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTWK02 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTWK03 DD  SPACE=(CYL,2),UNIT=SCRPK                          
//SORTIN   DD  DSN=CKBASE.EXUBBAPM.POLMAST.CLTOBO.EXTMV,         
//             DISP=SHR,                                         
//             DCB=BUFNO=20                                      
//CTL1OFT1 DD DSN=CKBASE.EXUBBAPM.PLDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT2 DD DSN=CKBASE.EXUBBAPM.CDDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT3 DD DSN=CKBASE.EXUBBAPM.MIDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT4 DD DSN=CKBASE.EXUBBAPM.MODIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT5 DD DSN=CKBASE.EXUBBAPM.FADIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL1OFT6 DD DSN=CKBASE.EXUBBAPM.LSDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//CTL2OFT7 DD DSN=CKBASE.EXUBBAPM.PTDIM.POLMAST.MV.DATA,         
//             DISP=(,CATLG,DELETE),                             
//             UNIT=DEVPK,                                       
//             SPACE=(TRK,(5,5),RLSE),                           
//             DCB=(*.SORTIN)                                    
//SYSIN    DD   *  
SUM FIELDS=NONE 
SORT FIELDS=(5,396,BI,A)
OUTFIL FILES=CTL1OFT1,INCLUDE=(5,2,CH,EQ,C'PL')                        
OUTFIL FILES=CTL1OFT2,INCLUDE=(5,2,CH,EQ,C'CV')                        
OUTFIL FILES=CTL1OFT3,INCLUDE=(5,2,CH,EQ,C'FA')                        
OUTFIL FILES=CTL1OFT4,INCLUDE=(5,2,CH,EQ,C'MI')                        
OUTFIL FILES=CTL1OFT5,INCLUDE=(5,2,CH,EQ,C'MO')                        
OUTFIL FILES=CTL1OFT6,INCLUDE=(5,2,CH,EQ,C'PT')
/*                                                               
// 
//* 
                                                                   
//* 
//* 
//*
//GDGSTEP3   EXEC PGM=IEFBR14
//DD01     DD DSN=CKBASE.EXUBBAPM.POLMAST.CLTOBO.EXTMV,                
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(12,33),RLSE),UNIT=SYSDA,
//            DCB=(DSORG=PS,RECFM=VB,LRECL=27998)
//*GDGDEL     DD  DSN=CKBASE.EXUBBAPM.POLMAST.CLTOBO.EXTMV,            
//*               DISP=(NEW,CATLG,DELETE),
//*               DCB=(DSORG=PS,RECFM=VB,LRECL=27998,BLKSIZE=27998)
//
//* 

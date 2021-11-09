C***********************************************************************
C*                                                                      
C*    PROGRAM LOGFT                                                     
C*                                                                      
C*    VERSION 1 SUPPLIED BY ORNL.                                       
C*    VERSION 2A(43) AS OF  6-DEC-77. CONVERT TO MACHINE DEPENDENT CODE.
C*    VERSION 2A(44) AS OF  3-JAN-78. FIX IF ERROR.                     
C*    VERSION 3(45)  AS OF  4-JAN-78. REDUCE TO SINGLE PRECISION.       
C*    VERSION 3(50)  AS OF 31-JAN-78. REPLACE MIN1 WITH AMIN1.          
C*    VERSION 3(51)  AS OF  1-MAR-78. MATCH NEW (NOV. '77) NDP PGM.     
C*    VERSION 3(52)  AS OF 10-JUL-78. BRING UP TO NEW FORSTR LEVEL.     
C*    VERSION 3(53)  AS OF  2-AUG-78. FIX STMT 6160 TO BE F7.3 NOT F6.3.
C*    VERSION 3(54)  AS OF 12-DEC-79. HANDLE MISSING UNCERT. FOR INTENS.
C*    VERSION 3(55)  AS OF 12-DEC-79. CORRECT STMT 1274 FOR VAR. LEN.   
C*    VERSION 3(56)  AS OF 12-DEC-79. CHANGE K,L,M+ TO CK,CL,CM+.       
C*    VERSION 3(57)  AS OF 12-DEC-79. RETAIN PREC. OF IE, IB.           
C*    VERSION 4(60)  AS OF 11-FEB-81. ADD SUBR. CNVUXS AND CHANGE CALLS.
C*    VERSION 4(61)  AS OF 26-MAY-81. REMOVE CNVUXS. CNVU2S NOW DOES IT.
C*    VERSION 4(62)  AS OF 26-MAY-81. REPLACE CALL TO INSERT WITH PUTIN.
C*    VERSION 4(63)  AS OF 27-MAY-81. USE DIALOG=LINE FOR DEC.          
C*    VERSION 5(64)  AS OF 20-AUG-81. PLACE NUCID ON 2B AND 2E CARDS.   
C*    VERSION 5(65)  AS OF  2-SEP-81. REMOVE LAST $ ON CONT CARDS.      
C*    VERSION 5(66)  AS OF 23-MAY-84. FIX NEW E CARD GENERATION.        
C*    VERSION 6               JAN-86. CONVERT TO FORTRAN 77             
C*    VERSION 6(1)          6-AUG-86. Added VAX MDC                     
C*    VERSION 6(2)          2-DEC-86. Added IbmPC MDC. OVERFLOW IN DEINT
C*                                    FIXED.                            
C*    VERSION 6(3)          2-MAR-87. 2 B, 2 E to S B, S E for cont.    
C*                           records                                    
C*    VERSION 6(4)          2-NOV-87. VAX mdc READONLY in OPEN data file
C*    VERSION 7             6-MAR-89. Retain alpha uncertainties.  PN   
C*                           records. Mutiple Parent cards. RADLST's    
C*                           integration some output format mods. Clean 
C*                           up code etc.                               
C*    VERSION 7(1)  20-APR-89.  BKL,BL1L (Z<5) data values corrected by 
C*    VERSION 7(2)     AUG-89.  further fortran cleanup.                
C*    VERSION 7(3)  14-Dec-89.  Correct convergence problems in QROMB   
C*                              Fuction F modified.  (T.Burrows)        
C*    VERSION 7(4)   6-JUN-90.  Convert to IBM PC versin.  Some cleanup.
C*    VERSION 7(5)  14-SEP-90.  IBM version correction. IBONLY set      
C*                           condition corrected.                       
C*    VERSION 7(6)  15-OCT-90   Old integration package. Created PROCE  
C*                     and WECARD subroutines from sections of MAIN.    
C*                     Corrected TYPSTR declaration, Q(NUMP) bound error
C*    VERSION 7(7)  22-MAY-91   Correct value assignments to new card.  
C*    VERSION 7(8)  26-NOV-91   Correct SKIP1 setting when level E nonnu
C*    VERSION 7(9)  14-Oct-92.  Restored RADLST integration package     
C*                           (run-time stack overflow problem on IBM-PC 
C*                           solved with "/ST:4096" on link).           
C*                              Corrected statement out of order in     
C*                           function F(W) and use of consecutive       
C*                           arithmetic operators in subroutine POLINT  
C*                              Restored some variable declarations     
C*                           (commented out) to correspond to commented 
C*                           out sections of code                       
C*                              Added Machine coding for ANS            
C*                           (T.W. Burrows)                             
C*    VERSION 7(10) 16-Dec-92.  Tightened convergence tests from        
C*                            EPS=1.E-5, EPS1=10.*EPS to EPS=5.E-6,     
C*                            EPS1=EPS and rewrote check on convergence 
C*                            in QROMB after intercomparison with CML   
C*                            code BETA.                                
C*                              Finished typing all variables.          
C*                              Changed heurestic check on W0 from 1.2  
C*                            to 2.0                                    
C*                              Protected against floating overflows in 
C*                            calculating DETOP                         
C*                              Rewrote logic in calculating PINT and   
C*                            EINT to protect against unrealistic       
C*                            uncertainties when ETOP large or machine  
C*                            roundoff when ETOP small                  
C*                              Corrected integer function IPREC to     
C*                            account for floating point notation       
C*                              Corrected logic for outputing IB and    
C*                            IE when large uncertainties               
C*                            (T.W. Burrows)                            
C*    VERSION 7(11) 17-Feb-93.  Finished correcting convergence problems
C*                            by judicious addition of double precision 
C*                              Corrected:                              
C*                            a. problem when half-life units at end of 
C*                              field and neglect of "?" in T field     
C*                            b. neglect of PN record                   
C*                            c. neglect of parent and level energies of
C*                              the form "0+X"                          
C*                            d. overflow on branching ratio output     
C*                              Added:                                  
C*                            a. more terminal output to notify user of 
C*                              possible problems                       
C*                            b. check of NB*BR versus NBBR             
C*                            c. warning when NB=1/BR assumed           
C*                            d. Check for "E" or "B" continuation      
C*                              records other then "2" or "S"           
C*                            e. Improved algorithms for significant    
C*                              digits                                  
C*                            f. Minor cleanup                          
C*    VERSION 7(12) 10-May-93 a. Added checking on NUCID                
C*                            b. Corrected calculation of A for Z>105   
C*                            c. Corrected logic error which did not    
C*                               suppress new output when non-numeric   
C*                               uncertainty on E(level)                
C*                            d. Added check for unplaced or redundant  
C*                               E or B records                         
C*                            e. Added check for calculated transition  
C*                              energies less than or equal to 0        
C*                            f. Delinted using FLINT 2.83              
C*                            (TWB)                                     
C*    VERSION 7(13) 03-Aug-93 Changed environment variable for VAX      
C*                              version. (TWB)                          
C*    VERSION 7(14) 03-Sep-93 Commented out IF statement after call to  
C*                              WECARD. For EC only this resulted in    
C*                              the last calculated LOGFn and LOGFnT to 
C*                              be reported and values on E record to   
C*                              not be modified. The error was          
C*                              introduced in version 7(11) [17-Feb-93].
C*    Version 7(15) 25-Mar-96 a. Outputing a blank "S B" card when      
C*                              linked with the latest version of       
C*                              NSDFLIB - caused by a correction in the 
C*                              CNVU2S routine in NSDFLIB 1.4 -         
C*                              corrected all calls for format 2 of     
C*                              CNVU2S by LOGFT                         
C*                            b. Corrected problem when blank T1/2 field
C*                              on P record                             
C*                            c. Cleaned out redundant coding in WECARD 
C*                              by introducing ADDFRAC subroutine       
C*                            d. Other minor output cleanup             
C*                            e. Corrected problems noted by AIX XL     
C*                              Fortran Compiler 0.3.02.0002.0000       
C*    Version 7.15a 13-Apr-99 a. Check for and skip Ionized Atom        
C*                              datasets                                
C*                            b. Y2K compliance                         
C*                            c. Corrected an arithmetic overflow in    
C*                              calculating dtp                         
C*                            d. Check uniqueness. If not allowed or    
C*                              1U or 2U, added warning to report and   
C*                              terminal output and comment to new file 
C*    Version 7.2    7-Feb-01 Added code to enable program to run on    
C*                            Linux OS using GNU f77. Save statements   
C*                            were added and NBBR was initialized.      
C*    VERSION 7.3    20-Apr-16 Took out duplicate date_20 as is
C*                             used in nndclib, and ensured could
C*                             be compiled with gfotran.
C*                                                                      
C*    REFER ALL COMMENTS AND INQUIRIES TO                               
C*    NATIONAL NUCLEAR DATA CENTER                                      
C*    BUILDING 197D                                                     
C*    BROOKHAVEN NATIONAL LABORATORY                                    
C*    UPTON, NEW YORK 11973                                             
C*    TELEPHONE 631-344-2901                                            
C*                                                                      
C***********************************************************************
C*                                                                      
C*                                                                      
C  PROGRAM LOGFT FOR DATA-BANK DATA SETS.                               
C                                                                       
C  THIS PROGRAM CALCULATES LOG FT FOR BETA DECAY.                       
C  FOR CAPTURE DECAY IT ALSO CALCULATES L/K RATIO.                      
C  FOR POSITRON DECAY IT ALSO CALCULATES E/B+ RATIO.                    
C  IT WILL DO SPECIAL CALCULATIONS FOR FIRST AND SECOND                 
C  FORBIDDEN UNIQUE.                                                    
C  ALL OTHER CATEGORIES ARE TREATED AS ALLOWED.                         
C                                                                       
C  THE PROGRAM WAS CONVERTED FROM FORTRAN IV TO 77.                     
C                                                                       
C  THE FOLLOWING SUBROUTINES ARE SUPPLIED WITH THE MAIN PROGRAM         
C                                                                       
C    SETUP   SETS UP SOME OF THE VALUES USED IN INTEGRATION             
C    F       THE INTEGRAND                                              
C    GAMMA   GAMMA FUNCTION (COMPLEX ARGUMENT)                          
C    CAPTUR  COMPUTES ELECTRON CAPTURE                                  
C    READWV  READS IN K, L WAVE FUNCTIONS                               
C    WAVEFN  RETURNS WAVE FUNCTION TO CAPTUR                            
C                                                                       
      PROGRAM LOGFT                                                     
C                                                                       
      INTEGER NF                                                        
      COMMON /FCOMM/NF                                                  
C                                                                       
      REAL EPS,EPS1                                                     
      COMMON/PREC1/EPS,EPS1                                             
C                                                                       
      COMMON /CARDIM/CARD,CARD2,STARS                                   
      CHARACTER*80  CARD                                                
      CHARACTER*80  CARD2                                               
      CHARACTER* 8  STARS                                               
C                                                                       
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          
     1    IBONLY,ZERUNC,EK,NUMP                                         
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             
      REAL    DPERC, DPINT,DEINT                                        
      REAL    PERC, PINT, EINT                                          
      REAL    EK(5)                                                     
C                                                                       
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              
      CHARACTER* 2  TYPE                                                
      CHARACTER     ATI*10,  DATI*2                                     
      CHARACTER*2   UNCERT                                              
C                                                                       
      COMMON /WENUM/ AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM,     
     1    FCLP, FCMNM, FCMNO, FCMNP,NPOS,ETOP                           
      INTEGER NPOS                                                      
      REAL   AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM, FCLP,       
     1    FCMNM, FCMNO, FCMNP,ETOP                                      
C                                                                       
C-REAL VARIABLES.                                                       
C                                                                       
      REAL A, AREA1U                                                    
      REAL BR                                                           
      REAL CAPTUR                                                       
      REAL DBR, DE(5), DEBAR, DEE,  DEIS,                               
     1    DELEV, DELF, DEK(5), DETOP, DFLT,                             
     2    DLAREA, DLFT, DNB, DQ(5), DT(5), DTP,DW0                      
      REAL E(5), EAR1U, EAREA, EAREAM, EAREAP, EBAR, EBARM,             
     1    EBARP, EBARR, EIS, EKTOP,                                     
     2    ELEV, ELMASS,                                                 
     3    ETOPL, ETOPM, ETOPP                                           
      REAL FC, FC1,                                                     
     1    FCM, FCMN1, FCP, FLOG, FLOGF,                                 
     2    FLOGM, FLOGP, FLOGT, FT, FTOUT                                
      REAL NB                                                           
      REAL Q(5)                                                         
      REAL THALF(5), TMULT, TP, TUL(11)                                 
      REAL W0                                                           
      REAL ZL, NBBR, DNBBR                                              
      REAL PINTM,PINTP                                                  
C                                                                       
C-External functions                                                    
      INTEGER Indexf,Iprec,LENSTR,Rlscn,TYPSTR                          
      REAL FNEW,Valstr,XF                                               
      EXTERNAL FNEW,Indexf,Iprec,LENSTR,Rlscn,TYPSTR,Valstr,XF          
C-Intrinsic functions                                                   
      Integer IABS,INDEX,INT,MAX0                                       
      Real ALOG10                                                       
      Intrinsic ALOG10,IABS,INDEX,INT,MAX0                              
C                                                                       
C-INTEGER VARIABLES.                                                    
C                                                                       
      INTEGER I, IE, IK, IPRINT, ISKIP,                                 
     1    ISKIP1,iskip2,lprec                                           
      INTEGER J                                                         
      INTEGER INUMP                                                     
C                                                                       
C-STRING VARIABLES.                                                     
C                                                                       
      CHARACTER* 1  C6                                                  
      CHARACTER* 3  C79                                                 
      CHARACTER* 1  C8                                                  
      CHARACTER*80  CARDI                                               
      CHARACTER*80  LINE                                                
      Character*80 crdcom                                               
      CHARACTER*10  STA                                                 
      CHARACTER* 6  STB                                                 
      CHARACTER* 5  STR                                                 
      CHARACTER* 2  UT(11)                                              
      CHARACTER*11   XDATE                                              
      CHARACTER     AN(5)*12, DAN(5)*2                                  
C                                                                       
      LOGICAL       PDONE,FIRSTI,DONBBR,somskp,levfnd                   
      Logical dowarn                                                    
C                                                                       
C    UNCERT    REMEMBERS THE NONNUMERICAL UNCERTAINTY OF INPUT          
C    PDONE     END OF PARENT RECORD FOR THE DATASET                     
C    NUMP      NO OF PARENT RECORDS                                     
C                                                                       
C-INITIALIZATION SECTION.                                               
C                                                                       
      DATA TUL/31536000.,86400.,3600.,60., 1., 1., 1., 1., 1., 1., 1./  
C     Right justify single character symbols to allow for possibility   
C       of symbol being last character in T field (TWB 921224)          
      DATA UT /' Y',     ' D',  ' H', ' M',' S','MS','US','NS','PS',    
     1         'FS','AS'/                                               
      DATA ELMASS, ZL/0.511004, 1./                                     
C                                                                       
 9000 FORMAT(A)                                                         
 9001 FORMAT('1'/98X, A)                                                
 9002 FORMAT(1X, A)                                                     
 9004 FORMAT('+', 100X,A)                                               
 9005 FORMAT('0', 20X,A )                                               
 9008 FORMAT(20X, A)                                                    
 9009 FORMAT('0', 8X, 'TRANSITION(KEV)=', A, 1X, A,                     
     1    ', T1/2(SEC)=', A, 1X, A, ', BETA BRANCHING(%)=',             
     2    A, 1X, A, ', PARTIAL T1/2(SEC)=', A, 1X, A)                   
 9010 FORMAT('0', 8X, 'TRANSITION(KEV)=', A, 1X, A,                     
     1    ', T1/2(SEC)=', A, 1X, A, ', BRANCHING(%)=',                  
     2    A, 1X, A, ', PARTIAL T1/2(SEC)=', A, 1X, A)                   
 9011 FORMAT(20X,A , A, 1X, A)                                          
 9012 FORMAT(45X,A )                                                    
 9013 FORMAT(20X, A, I1, A, F6.3,A)                                     
 9016 FORMAT (20X, A, F7.3)                                             
 9017 FORMAT(20X, 'CAPTURE TO POSITRON RATIO =', 1PE10.3, '+-', E9.2,   
     1    10X, 'LOG(E/B+)=', 0PF7.3, 10X, 'K/B+=', 1PE10.3)             
 9018 FORMAT(20X, 'POSITRON INTENSITY = ', 1PE8.2, '+-', 1PE9.1, ' ,',  
     1    10X, 'ELECTRON CAPTURE INTENSITY =', E9.2, '+-', E9.1, ' ,')  
 9020 FORMAT(20X, 'E=', F8.2, 5X, 'LOG F', I1, '=', F6.3, '+-', F6.3)   
 9021 FORMAT(' LOG F', I1, 'T = ', F6.3, '+-', F6.3, 5X, 'F', I1, 'T=', 
     1    E12.5)                                                        
 9022 FORMAT('+', 50X, 'AVERAGE BETA(+-) ENERGY=', F8.2, '+-', F7.3,    
     1    4X, 'EBAR/E = ', F7.4, /)                                     
 9023 FORMAT('0', A, 5X, A)                                             
 9024 FORMAT('+', 85X, A)                                               
 9025 FORMAT(1X, A, 10X, A)                                             
 9026 FORMAT(////,98X,A)                                                
C                                                                       
      stars='********'                                                  
      crdcom='*****'                                                    
      dowarn=.FALSE.                                                    
      nbbr=0.                                                           
C                                                                       
      Write(6,9000)' LOGFT Version 7.3 [ 20-Apr-16]'                     
C                                                                       
C+++MDC+++                                                              
C...ANS                                                                 
C/99901 FORMAT(' INPUT DATA SET FILE (TEST.DAT):    ')                  
C/99902 FORMAT(' OUTPUT REPORT FILE (LOGFT.RPT):     ')                 
C/99903 FORMAT(' DATA TABLE (LOGFT.DAT): ')                             
C/99904 FORMAT(' OUTPUT DATA SET FILE (LOGFT.NEW):   ')                 
C...VAX, DVF, UNX
99901 FORMAT(' INPUT DATA SET FILE (data.tst):    ',$)                  
99902 FORMAT(' OUTPUT REPORT FILE (logft.rpt):     ',$)                 
99903 FORMAT(' DATA TABLE (logft.dat): ',$)                             
99904 FORMAT(' OUTPUT DATA SET FILE (logft.new):   ',$)                 
C---MDC
99900 FORMAT(A)                                                         
C+++MDC
C...ANS, UNX
      WRITE(6,99901)                                                    
      READ(5,99900) LINE                                                
      IF(LINE.EQ.' ') LINE='data.tst'                                   
      OPEN(UNIT=35, ACCESS='SEQUENTIAL' ,STATUS='OLD',                  
     1     FILE=LINE)                                                   
      WRITE(6,99902)                                                    
      READ(5,99900) LINE                                                
      IF(LINE.EQ.' ') LINE='logft.rpt'                                  
      OPEN(UNIT=36, ACCESS='SEQUENTIAL', STATUS='UNKNOWN',              
     1     FILE=LINE)                                                   
      WRITE(6,99903)                                                    
      READ(5,99900) LINE                                                
      IF(LINE.EQ.' ') LINE='logft.dat'                                  
      OPEN(UNIT=20, ACCESS='SEQUENTIAL' ,STATUS='OLD',                  
     1     FILE=LINE)                                                   
      WRITE(6,99904)                                                    
      READ(5,99900) LINE                                                
      IF(LINE.EQ.' ') LINE='logft.new'                                  
      OPEN(UNIT=21, ACCESS='SEQUENTIAL' ,STATUS='UNKNOWN',              
     1     FILE=LINE)                                                   

      XDATE=' '                                                         
C+++MDC+++                                                              
C...DVF, VAX, UNX                                                       
      CALL DATE_20(XDATE)                                               
C...ANS                                                                 
C---MDC---                                                              
C                                                                       
C-PROGRAM BEGINS BY READING WAVE FUNCTION CARDS IN READWV.              
C-THE LAST OF THESE CONTAINS BLANKS.                                    
C                                                                       
      CALL READWV                                                       
C                                                                       
C-NTRY IS THE NUMBER IF ITERATIONS TO USE IN THE INTEGRATION BEFORE     
C-GIVING UP.                                                            
C-PREC IS THE PRECISION DESIRED.                                        
C                                                                       
C    Precision of 0.000001 is too strict for very low W. Old version    
C      of LOGFT using SIMCON used 0.0001 so comprimise                  
C      EPS=1.0E-6                                                       
C      EPS1=100.*EPS                                                    
C     Changed from EPS=1.E-5, EPS1=10.*EPS to EPS=5.E-6,EPS1=EPS        
C       (TWB. 921216)                                                   
      EPS=5.0E-6                                                        
      EPS1=0.1*EPS                                                      
C      PREC = 1.0E-6                                                    
      WRITE(36, 1999)                                                   
 1999 FORMAT('1PROGRAM  L O G F T  VERSION 7.2 AS OF 7-Feb-2001.')      
      IPRINT = 1                                                        
      INUMP=0                                                           
      FIRSTI=.TRUE.                                                     
      iskip=0                                                           
      somskp=.FALSE.                                                    
      levfnd=.FALSE.                                                    
C                                                                       
C-CARD FORMAT.                                                          
C                                                                       
  100 READ(35, 9000, END=1265) CARD                                     
      I = TYPSTR(CARD(1:1))                                             
      IF (I .LT. 0 .OR. I .EQ. 2) GO TO 900                             
      C6=CARD(6:6)                                                      
      C79=CARD(7:9)                                                     
      IF (C6 .NE. ' ' .AND. C6 .NE. '1') GO TO 600                      
      C8=CARD(8:8)                                                      
C     "PN" record was never being read since check was only on column   
C       7 being blank (TWB. 930104)                                     
      IF(CARD(7:7) .NE. ' ')THEN                                        
         IF(CARD(7:8) .NE. 'PN')GO TO 900                               
      ENDIF                                                             
      IF(CARD(1:3) .EQ. '   ') GO TO 900                                
C                                                                       
C   I CARD                                                              
C   ======                                                              
      IF(C79 .EQ. '   ') THEN                                           
         BR = 1.                                                        
         DBR = 0.                                                       
         NB = 1.                                                        
         DNB = 0.                                                       
         NUMP=1                                                         
         PDONE=.FALSE.                                                  
C        Added warning message to terminal output when data skipped     
C          (TWB. 930212)                                                
         If(.NOT.firsti)Then                                            
            If(iskip .gt. 0)Then                                        
               Write(6,9002)'***** Data '//                             
     2           'set will not be modified - See LOGFT report'          
            Else                                                        
               If(somskp)Write(6,9002)'***** Some records '//           
     2           'will not be modified - See LOGFT report'              
            Endif                                                       
         Endif                                                          
         iskip=0                                                        
         somskp=.FALSE.                                                 
         levfnd=.FALSE.                                                 
         IF(FIRSTI) THEN                                                
            WRITE(36,9026) XDATE                                        
            FIRSTI=.FALSE.                                              
         ELSE                                                           
            WRITE(36, 9001) XDATE                                       
         ENDIF                                                          
         WRITE(36, 9002) CARD                                           
C        Added terminal output of DSID so that messages would be in     
C          context (TWB. 930212)                                        
         Write(6,9002)'Processing=====>'//card(1:39)                    
C        Check for Ionized Atom dataset and skip                        
         If(INDEX(card(10:39),' DECAY').GT.0 .AND.                      
     2     (INDEX(card(10:39),'[').GT.0                                 
     3     .AND. INDEX(card(10:39),'[')                                 
     4     .LT.INDEX(card(10:39),' DECAY')))Then                        
            Write(36,9002)'***** Ionized Atom dataset - skipping'       
            iskip=1                                                     
         EndIf                                                          
         STR=CARD(1:5)                                                  
         i=Rlscn(str,1,A)                                               
         If(i .EQ. 6)Then                                               
            If(INDEX(str(1:3),' ') .NE. 0)Then                          
               Write(36,9002)'***** Error in NUCID - Z=0 set'           
               iskip=1                                                  
            Else                                                        
               A=Valstr(str(1:3))                                       
            Endif                                                       
         Else If(i .NE. 4)Then                                          
            Write(36,9002)'***** Error in NUCID - Z=0 set'              
            iskip=1                                                     
         Else                                                           
            CALL IZEL(STR(4:5),IZ)                                      
            If(iz .EQ. -1)Then                                          
               Write(36,9002)'***** Element not identified - Z=0 set'   
               iskip=1                                                  
            Endif                                                       
         Endif                                                          
         If(iskip .EQ. 1)iz=0                                           
         DO 120 I=1,5                                                   
            Q(I) = 0.                                                   
  120    CONTINUE                                                       
         GOTO 900                                                       
      ENDIF                                                             
C                                                                       
C   COLUMNS 8,9, HAVE SOME LETTERS                                      
C                                                                       
C    P CARD                                                             
C    ======                                                             
C                                                                       
      IF (C8 .EQ.'P') THEN                                              
C                                                                       
C        ISKIP value reflects that of last p card if there are multiple 
C                                                                       
         WRITE(36, 9002) CARD                                           
         STA=CARD(10:19)                                                
         STB=CARD(20:21)                                                
         CALL CNVS2U(STA,STB,EIS,DEIS)                                  
C        Added check for non-numeric parent energies (TWB. 930212)      
         Call Lbsup(sta)                                                
         Call Lbsup(stb)                                                
         If(Typstr(sta) .EQ. 2 .OR. Typstr(sta) .EQ. -1                 
     2     .OR. Typstr(stb) .EQ. 2 .OR. Typstr(stb) .EQ. -1)Then        
            Write(36,9002)'Parent energy value uncertain - skip output' 
            iskip=1                                                     
         Endif                                                          
         STA=CARD(65:74)                                                
         STB=CARD(75:76)                                                
         CALL CNVS2U(STA,STB,Q(NUMP),DQ(NUMP))                          
C                                                                       
C   if DQP (DQ) field has non numeric uncertainty, do not output new rec
C                                                                       
         IF(STB .NE. ' ' .AND. DQ(NUMP).EQ.0.)THEN                      
            ISKIP=1                                                     
            WRITE(36,9002)                                              
     1          ' DQP on P-card with non numeric uncert. - skip output' 
         ENDIF                                                          
         Q(NUMP) = Q(NUMP) + EIS                                        
         DQ(NUMP) = SQRT(DQ(NUMP) * DQ(NUMP) + DEIS * DEIS)             
         STA=CARD(40:49)                                                
         STB=CARD(50:55)                                                
C                                                                       
         Call Lbsup(sta)                                                
         I=LENSTR(STA)                                                  
C        Check for blank T1/2 field                                     
         If(i .LE. 0)Then                                               
            Write(6,9002)'No T1/2 given. 1 S assumed'                   
            Write(36,9004)'NO T1/2 GIVEN. *******'                      
            Write(36,FMT='(101X,A)')'1 S ASSUMED'                       
            sta='1 S'                                                   
            i=3                                                         
            iskip=1                                                     
         EndIf                                                          
C        Format manual allows "?" after half-life (TWB. 921224)         
         IF(STA(I:I) .EQ. '?')THEN                                      
            If(i .GT. 1)Then                                            
               WRITE(6,9002)'NOTE UNCERTAIN T1/2 ASSIGNMENT'            
               WRITE(36, 9004) 'NOTE T1/2 ASSIGNMENT? *******'          
               I=LENSTR(STA(1:I-1))                                     
            Else                                                        
C              Check for missing T1/2                                   
               Write(6,9002)'No T1/2 given. 1 S assumed'                
               Write(36,9004)'NO T1/2 GIVEN. *******'                   
               Write(36,FMT='(101X,A)')'1 S ASSUMED'                    
               sta='1 S'                                                
               iskip=1                                                  
            EndIf                                                       
         ENDIF                                                          
C        Added error messages to report file                            
         IF(STA(I:I).LT.'A' .AND. STA(I:I).GT.'Z') THEN                 
            WRITE(6,9002)                                               
     1           'NO UNITS GIVEN FOR LIFETIME--SECONDS ASSUMED'         
            WRITE(36, 9004) 'NO UNITS FOR T1/2--   *******'             
            WRITE(36,FMT='(101X,A)')'SECONDS ASSUMED'                   
            IK=5                                                        
            ISKIP=1                                                     
         ELSE                                                           
            DO 130 IK=11,1,-1                                           
               IF(INDEX(STA,UT(IK)).NE.0) GO TO 140                     
  130       CONTINUE                                                    
            WRITE(6,9002)                                               
     1           'HALF-LIFE UNIT NOT RECOGNIZED. SECONDS SUBSTITUTED'   
            WRITE(36, 9004) 'T1/2 UNIT NOT RECOGNIZED *****'            
            WRITE(36,FMT='(101X,A)')'SECONDS SUBSTITUTED'               
            ISKIP=1                                                     
            IK=5                                                        
         ENDIF                                                          
C                                                                       
  140    CALL CNVS2U(STA, STB, THALF(NUMP), DT(NUMP))                   
         TMULT=1.                                                       
         IF(IK.GT.5) TMULT=.001**(IK-5)                                 
         THALF(NUMP) = THALF(NUMP) * TUL(IK) * TMULT                    
         DT(NUMP) = DT(NUMP) * TUL(IK) * TMULT                          
         NUMP=NUMP+1                                                    
         GOTO 900                                                       
      ENDIF                                                             
C                                                                       
C-PICK UP BRANCHING RATIO INFORMATION FROM N-CARD.                      
C                                          ======                       
C   PN card has P in column 7                                           
C                                                                       
      IF(C8 .EQ.'N') THEN                                               
         IF(CARD(7:7) .EQ. 'P') THEN                                    
C                                                                       
C   PN CARD                                                             
C   =======                                                             
C                                                                       
            STA=CARD(42:49)                                             
C           Added cross check between NB*BR and NBBR (TWB. 930212)      
            IF(STA.NE.' ') THEN                                         
               STB=CARD(50:55)                                          
               CALL CNVS2U(STA, STB, NBBR,DNBBR)                        
               WRITE(36, 9002) CARD                                     
               IF(ABS(NBBR-NB*BR) .GT. 0.001)WRITE(36,FMT=              
     2           '(''+'',100X,''CHECK NB*BR='',F5.3,'' .NE. NBBR *'')') 
     4           NB*BR                                                  
            ELSE                                                        
               IF(BR .NE. 1. .AND.(ABS(NB*BR-1.) .GT. 0.001))           
     2           WRITE(36,FMT=                                          
     3           '(''+'',100X,''CHECK NB*BR='',F5.3,''     *******'')') 
     4           NB*BR                                                  
               WRITE(36, 9002) CARD                                     
            ENDIF                                                       
            DONBBR=.FALSE.                                              
         ELSE                                                           
C                                                                       
C   N CARD                                                              
C                                                                       
C    BR field                                                           
C                                                                       
            WRITE(36, 9002) CARD                                        
            STA=CARD(32:39)                                             
            IF (STA .NE. ' ') THEN                                      
                STB=CARD(40:41)                                         
                CALL CNVS2U(STA, STB, BR, DBR)                          
            ELSE                                                        
               BR=0.                                                    
               DBR=0.                                                   
            ENDIF                                                       
            IF(BR .EQ. 0.)THEN                                          
               WRITE(36,9002)                                           
     1             ' BR=0. reset to 1. - skip output'                   
               ISKIP=1                                                  
               BR=1.0                                                   
            ENDIF                                                       
            STA=CARD(42:49)                                             
C                                                                       
C    NB field                                                           
C                                                                       
            IF (STA .EQ. ' ') THEN                                      
                NB = 1. / BR                                            
C              Changed from DBR = 0. to DNB = 0. and added warning      
C                message (TWB. 921224)                                  
                DNB = 0.                                                
                WRITE(36, 9004) 'NB ASSUMED 1/BR       *******'         
                GOTO 900                                                
            ENDIF                                                       
C                                                                       
C-IF STRING 'NR' IS IN NB FIELD, THEN ASSUME SAME                       
C-NORMALIZATION AS FOR GAMMAS.                                          
C                                                                       
            CALL SQZSTR(STA, ' ' )                                      
            IF (STA(1:2) .EQ. 'NR') THEN                                
C              Not allowed in manual (TWB. 921224)                      
               WRITE(36, 9004) '"NR" IN NB NOT ALLOWED ******'          
                STA=CARD(22:29)                                         
                STB=CARD(30:31)                                         
            ELSE                                                        
                STB=CARD(50:51)                                         
            ENDIF                                                       
            CALL CNVS2U(STA, STB, NB, DNB)                              
            DONBBR=.TRUE.                                               
C           Replace ambiguous and misleading message with something     
C             more informative and move to after PN record (TWB. 930104)
C            WRITE(36, 9004) 'NOTE BRANCHING RATIO  *******'            
         ENDIF                                                          
         GO TO 900                                                      
      ENDIF                                                             
C                                                                       
C   G,L,B OR E RECORD(NOT P)                                            
C   skip other cards                                                    
C                                                                       
      IF (C8 .NE. 'L' .AND. C8. NE. 'E' .AND.                           
     1    C8 .NE. 'B') GOTO 900                                         
C                                                                       
C  no more parent record                                                
C                                                                       
      IF(.NOT. PDONE) THEN                                              
         NUMP=NUMP-1                                                    
         PDONE=.TRUE.                                                   
      ENDIF                                                             
C  IF last Q value is 0, error message and ISKIP set                    
      IF (NUMP.EQ.0) THEN                                               
         IF (ISKIP .EQ. 0) WRITE(36, 9005)                              
     1             'Q-VALUE NOT GIVEN. MISSING P-CARD.'                 
         ISKIP = 1                                                      
         GOTO 900                                                       
      ENDIF                                                             
      IF (Q(NUMP) .EQ. 0.) THEN                                         
         IF (ISKIP .EQ. 0) WRITE(36, 9005)                              
     1             'Q-VALUE NOT GIVEN ON P-CARD.'                       
         ISKIP = 1                                                      
         GOTO 900                                                       
      ENDIF                                                             
      IF (C8 .EQ. 'L') THEN                                             
C                                                                       
C-PROCESS LEVEL RECORD. L-RECORD                                        
C                       ========                                        
C        Added cross check between NB*BR and NBBR (TWB. 930212)         
         IF(DONBBR)THEN                                                 
            IF(BR .NE. 1. .AND.(ABS(NB*BR-1.) .GT. 0.001))WRITE(36,FMT= 
     2        '(''+'',100X,''CHECK NB*BR='',F5.3,''     *******'')')    
     3        NB*BR                                                     
            DONBBR=.FALSE.                                              
         ENDIF                                                          
         INUMP=0                                                        
         ISKIP1 = 0                                                     
         iskip2=0                                                       
         levfnd=.TRUE.                                                  
         WRITE(36, 9002) CARD                                           
         STA=CARD(10:19)                                                
         STB=CARD(20:21)                                                
         CALL CNVS2U(STA, STB, ELEV, DELEV)                             
C                                                                       
C   if level energy value contains non numeric info, do not write new   
C   record                                                              
         Call Lbsup(sta)                                                
         Call Lbsup(stb)                                                
C        Program was not picking up "0+X", etc. (TWB. 930212)           
         If(Typstr(sta) .EQ. 2 .OR. Typstr(sta) .EQ. -1                 
     2     .OR. Typstr(stb) .EQ. 2 .OR. Typstr(stb) .EQ. -1)Then        
            ISKIP2=1                                                    
            somskp=.true.                                               
            WRITE(36,9002)                                              
     1        'Level energy value uncertain - skip output'              
         ENDIF                                                          
C                                                                       
C-CALCULATE BRANCH ENERGY FROM Q-VALUE.                                 
C                                                                       
         DO 200 I=1,NUMP                                                
            E(I) = Q(I) - ELEV                                          
            DE(I) = SQRT(DQ(I) * DQ(I) + DELEV * DELEV)                 
            EK(I) = E(I)                                                
            DEK(I) = DE(I)                                              
            E(I) = E(I) * 0.001                                         
            DE(I) = DE(I) * 0.001                                       
  200    CONTINUE                                                       
         GOTO 900                                                       
      ENDIF                                                             
C                                                                       
C   B or E card                                                         
C                                                                       
      CARDI=CARD                                                        
      IBONLY=0                                                          
      ZERUNC=0                                                          
      IF (C8 .EQ. 'B') THEN                                             
C                                                                       
C-START PROCESSING B RECORD.                                            
C                  ========                                             
C                                                                       
         WRITE(36, 9000) '0'                                            
         If(.NOT.levfnd)Then                                            
            Write(36,9002)card                                          
            Write(36,9002)                                              
     2        '***** Unplaced B or redundant B for level ignored'       
            iskip1=1                                                    
            somskp=.TRUE.                                               
            Goto 900                                                    
         Endif                                                          
         levfnd=.FALSE.                                                 
         IZ = -1 * IABS(IZ)                                             
         TYPE=CARD(78:79)                                               
         STA=CARD(22:29)                                                
         STB=CARD(30:31)                                                
         CALL CNVS2U(STA, STB, PERC, DPERC)                             
         UNCERT=STB                                                     
C     NOTE IF DPERC = 0.0 (MISSING UNCERT)                              
C     Added special check for 100% feeding (TWB. 930212)                
         IF ((DPERC .EQ. 0.0 .AND. PERC .NE. 100.) .OR. STB .NE. ' ')   
     2     ZERUNC = 1                                                   
         jprec=Iprec(sta)                                               
         GOTO 300                                                       
C                                                                       
C-END PROCESSING B RECORD.                                              
C                                                                       
      ELSE                                                              
C                                                                       
C-START PROCESSING E RECORD.                                            
C                  ========                                             
C                                                                       
         If(.NOT.levfnd)Then                                            
            WRITE(36, 9000) '0'                                         
            Write(36,9002)card                                          
            Write(36,9002)                                              
     2        '***** Unplaced E or redundant E for level ignored'       
            somskp=.TRUE.                                               
            iskip1=1                                                    
            Goto 900                                                    
         Endif                                                          
         levfnd=.FALSE.                                                 
         CALL PROCE                                                     
      ENDIF                                                             
C                                                                       
                                                                        
C-END PROCESSING OF E RECORD.                                           
C                                                                       
C********************                                                   
C-EITHER B- OR EC/B+ COMES THROUGH HERE.                                
C   B CARD OR E CARD                                                    
C   ======    ======                                                    
C                                                                       
C   do the calculations below once for each parent record               
C                                                                       
  300 Continue                                                          
      INUMP=INUMP+1                                                     
      CARD2=' '                                                         
      ETOP = 0.                                                         
      DETOP = 0.                                                        
      ISKIP1=iskip2                                                     
      If(e(inump) .LE. 0.0)Then                                         
         Write(36,                                                      
     2     FMT='(''0'', 8X, ''TRANSITION(KEV)='', E8.2, 1X, E8.2,       
     2       '' less than or equal to 0 - Skipping calculation'')')     
     2     E(inump),DE(inump)                                           
         Write(36, 9023) cardi, 'OLD CARD'                              
         Write(36, 9024) 'OLD CARD KEPT'                                
         Write(21, 9000) cardi                                          
         somskp=.TRUE.                                                  
         iskip1=1                                                       
         IF(INUMP.LT.NUMP) GO TO 300                                    
         Goto 100                                                       
      Endif                                                             
      IF (PERC .LE. 0.) THEN                                            
C                                                                       
C-RECOVERY FROM MISSING BRANCHING RATIO.                                
C                                                                       
          WRITE(36, 9002)'  INTENSITY SET AT 100.'                      
          PERC = 100.                                                   
          EINT = PERC                                                   
          DEINT = 0.                                                    
          ISKIP1 = 1                                                    
          somskp=.TRUE.                                                 
      ENDIF                                                             
C                                                                       
C-CONVERT ENERGY TO MC ** 2 UNITS AND ADD REST MASS.                    
C                                                                       
      W0 = E(INUMP) / ELMASS + ZL                                       
      DW0 = DE(INUMP) / ELMASS                                          
C                                                                       
C-SET FLAG INITIALLY FOR ALLOWED TRANSITION.                            
C                                                                       
      IE = 0                                                            
C                                                                       
C-TEST IF FIRST FORBIDDEN UNIQUE OR SECOND FORBIDDEN UNIQUE.            
C                                                                       
      IF (TYPE .EQ. '1U') IE = 1                                        
      IF (TYPE .EQ. '2U') IE = 2                                        
C                                                                       
C-Check for other types which will be assumed to be allowed             
C                                                                       
      If(type.NE.'  ' .AND. ie.EQ.0)Then                                
         dowarn=.TRUE.                                                  
      Else                                                              
         dowarn=.FALSE.                                                 
         crdcom='*****'                                                 
      EndIf                                                             
C                                                                       
C-HALF-LIFE.                                                            
C                                                                       
      IF (THALF(INUMP) .EQ. 0.) THEN                                    
         WRITE(36, 9008)'HALF LIFE SET TO 1 S'                          
         ISKIP1 = 1                                                     
         somskp=.TRUE.                                                  
         THALF(INUMP) = 1.                                              
         DT(INUMP) = 0.                                                 
         FLOGT = 0.                                                     
      ENDIF                                                             
      IF(NBBR.EQ.0.) THEN                                               
         TP = 100. * THALF(INUMP) / ABS(PERC) / BR / NB                 
      ELSE                                                              
         TP= 100. * THALF(INUMP) /ABS(PERC) / NBBR                      
      ENDIF                                                             
      DTP = 0.                                                          
      IF (THALF(INUMP) .NE. 0.) THEN                                    
         dtp=dt(inump)/thalf(inump)                                     
         dtp=dtp*dtp                                                    
         IF(NBBR.EQ.0.) THEN                                            
            dtp=dtp+(dbr/br)*(dbr/br)+(dnb/nb)*(dnb/nb)                 
         ELSE                                                           
            dtp=dtp+(dnbbr/nbbr)*(dnbbr/nbbr)                           
         ENDIF                                                          
      ENDIF                                                             
      IF (PERC .NE. 0.) DTP = DTP + DPERC * DPERC / PERC /PERC          
      IF (DTP .NE. 0.) DTP = TP * SQRT(DTP)                             
C-LOG(E) = 0.43429                                                      
      FLOGT = ALOG10(TP)                                                
      DFLT = DTP * 0.43429 / TP                                         
C                                                                       
      CALL CNVU2S(EK(INUMP), DEK(INUMP), AN(1), 8, DAN(1), 2)           
      CALL CNVU2S(THALF(INUMP), DT(INUMP), AN(2), 12, DAN(2), 2)        
C     Corrected for overflow on branching ratio (TWB. 930212)           
      If(dperc .NE. 0)Then                                              
         CALL CNVU2S(PERC, DPERC, AN(3), 8, DAN(3), 2)                  
      Else                                                              
         lprec=-2                                                       
         If(ALOG10(perc) .GE. 1.)lprec=lprec-INT(ALOG10(perc))+1        
         Call Cnvu2s(perc,0.0,an(3),8,dan(3),lprec)                     
         dan(3)=uncert                                                  
      Endif                                                             
      CALL CNVU2S(TP, DTP, AN(4), 12, DAN(4), 2)                        
C                                                                       
C-IZ < 0 FOR B- DECAY.                                                  
C                                                                       
      IF(IZ.GT.0 .AND. IBONLY.NE.0) THEN                                
C                                                                       
C-ONLY POSITRON INTENSITY WAS GIVEN.                                    
C-EC INTENSITY WILL BE CALCULATED.                                      
C                                                                       
          WRITE(36, 9009)                                               
     1    (   AN(J)(1:8) ,DAN(J)(1:2) ,AN(J+1) ,DAN(J+1)(1:2),          
     3    J = 1, 3, 2)                                                  
      ELSE                                                              
C                                                                       
C-TOTAL BRANCHING WAS GIVEN.                                            
C                                                                       
          WRITE(36, 9010)                                               
     1    (   AN(J)(1:8), DAN(J)(1:2), AN(J+1), DAN(J+1)(1:2),          
     3    J = 1, 3, 2)                                                  
      ENDIF                                                             
      CALL CNVU2S(FLOGT, DFLT, AN(5), 8, DAN(5), 2)                     
      WRITE(36, 9011) 'LOG PARTIAL T1/2 =', AN(5), DAN(5)               
      IF (A .EQ. 0.) THEN                                               
         WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'               
         GOTO 900                                                       
      ENDIF                                                             
C                                                                       
C-TEST IF POSITRON OR NEGATRON.                                         
C-SET NPOS = -1 FOR B- DECAY.                                           
C                                                                       
      NPOS = -1                                                         
      IF (IE .EQ. 1) WRITE(36, 9012) 'FIRST-FORBIDDEN-UNIQUE'           
      IF (IE .EQ. 2) WRITE(36, 9012) 'SECOND-FORBIDDEN UNIQUE'          
C                                                                       
C-IZ < 0 FOR B- DECAY.                                                  
C-IZ = 0 IS AN ERROR.                                                   
C-IZ > 0 FOR EC, B+ DECAY.                                              
C                                                                       
      IF(IZ.EQ.0) THEN                                                  
         WRITE(36, 9004) 'Z VALUE MISSING. OLD CARD KEPT'               
         GOTO 900                                                       
      ENDIF                                                             
      IF(IZ.GT.0) THEN                                                  
C                                                                       
C-ELECTRON CAPTURE ALLOWED.                                             
C                                                                       
         FC = CAPTUR(IZ, W0, IPRINT, 0, FCK, FCL, FCMNO)                
         FC1 = 0.                                                       
         IF (IE .EQ. 0) THEN                                            
             FCP = CAPTUR(IZ, W0 + DW0, IPRINT, 0, FCKP, FCLP, FCMNP)   
             FCM = CAPTUR(IZ, W0 - DW0, IPRINT, 0, FCKM, FCLM, FCMNM)   
         ELSE                                                           
C                                                                       
C-CAPTURE FOR FIRST OR SECOND FORBIDDEN UNIQUE.                         
C                                                                       
             FC1 = CAPTUR(IZ, W0, IPRINT, IE, FCK, FCL, FCMN1)          
             FCP = CAPTUR(IZ, W0 + DW0, IPRINT, IE, FCKP, FCLP, FCMNP)  
             FCM = CAPTUR(IZ, W0 - DW0, IPRINT, IE, FCKM, FCLM, FCMNM)  
             DELF=ALOG10(FC1 / FC)                                      
             FC = FC1                                                   
             FCMNO = FCMN1                                              
             IF (IPRINT .NE. 0) WRITE(36, 9013) ' LOG(F', IE,           
     1        '/F0) FOR ELECTRON CAPTURE = ', DELF                      
         ENDIF                                                          
C                                                                       
C-TEST IF ENOUGH ENERGY FOR POSITRONS.                                  
C-SET NPOS = 0 FOR EC-ONLY DECAY.                                       
C                                                                       
         NPOS = 0                                                       
         AREA = 0.                                                      
         AREAP = 0.                                                     
         AREAM = 0.                                                     
         AREA1U = 0.                                                    
C                                                                       
         IF (W0 .LE. 3.01) THEN                                         
            AREA = AREA + FC                                            
            AREAP = AREAP + FCP                                         
            AREAM = AREAM + FCM                                         
            IF (AREA .LE. 0.) THEN                                      
C                                                                       
C-ERROR EXIT FOR NEGATIVE OR ZERO INTEGRATION.                          
C                                                                       
                WRITE(36, 9000)                                         
     1            '0   ZERO OR NEGATIVE INTEGRATION. OLD CARD KEPT'     
                ISKIP1 = 1                                              
                somskp=.TRUE.                                           
                GOTO 900                                                
            ENDIF                                                       
            GO TO 410                                                   
         ENDIF                                                          
C                                                                       
C-POSITRON DECAY SETUP IF B+ IS POSSIBLE.                               
C                                                                       
         W0 = W0 - 2.                                                   
C                                                                       
C-SET NPOS = +1 FOR COMBINED EC AND B+ DECAY.                           
C                                                                       
         NPOS = 1                                                       
      ENDIF                                                             
C                                                                       
C-INTEGRATION PROCEDURE USED FOR EITHER B+ OR B-.                       
C                                                                       
C-SET UP CONSTANTS TO BE USED IN INTEGRATION.                           
C                                                                       
      CALL SETUP(A, IZ, W0 + DW0, 0)                                    
C                                                                       
C-CALL INTEGRATION SUBROUTINE.                                          
C                                                                       
C      CALL SIMCON(ZL, W0+DW0, PREC, NTRY, AREAP, EAREAP, NOI, RSIM, R2)
C     Convergence problem solved so heurestic solution removed (TWB.    
C        930212)                                                        
      NF=1                                                              
      CALL QROMB(FNEW,ZL,W0+DW0,AREAP)                                  
      CALL QROMB(XF,ZL,W0+DW0,EAREAP)                                   
      CALL SETUP(A, IZ, W0 - DW0, 0)                                    
C      CALL SIMCON(ZL, W0-DW0, PREC, NTRY, AREAM, EAREAM, NOI, RSIM, R2)
      NF=1                                                              
      CALL QROMB(FNEW,ZL,W0-DW0,AREAM)                                  
      CALL QROMB(XF,ZL,W0-DW0,EAREAM)                                   
      CALL SETUP(A, IZ, W0, 0)                                          
C      CALL SIMCON(ZL, W0, PREC, NTRY, AREA, EAREA, NOI, RSIM, R2)      
      NF=1                                                              
      CALL QROMB(FNEW,ZL,W0,AREA)                                       
      CALL QROMB(XF,ZL,W0,EAREA)                                        
C                                                                       
C-TEST FOR CONVERGENCE OF INTEGRATION. for SIMCON                       
C                                                                       
C      IF (RSIM .GT. PREC)                                              
C     1     WRITE(36, 9014) RSIM, 'DESIRED PRECISION NOT REACHED'       
C                                                                       
C-CALCULATE AVERAGE ENERGY.                                             
C                                                                       
      IF (IE .NE. 0) THEN                                               
C                                                                       
C-INTEGRATE FOR FIRST OR SECOND FORBIDDEN UNIQUE.                       
C                                                                       
          CALL SETUP(A, IZ, W0 - DW0, IE)                               
C          CALL SIMCON(ZL, W0-DW0, PREC,NTRY,AREAM,EAREAM,NOI,RSIM,R2)  
          NF=1                                                          
          CALL QROMB(FNEW,ZL,W0-DW0,AREAM)                              
          CALL QROMB(XF,ZL,W0-DW0,EAREAM)                               
          CALL SETUP(A, IZ, W0 + DW0, IE)                               
C          CALL SIMCON(ZL, W0+DW0, PREC,NTRY,AREAP,EAREAP,NOI,RSIM,R2)  
          NF=1                                                          
          CALL QROMB(FNEW,ZL,W0+DW0,AREAP)                              
          CALL QROMB(XF,ZL,W0+DW0,EAREAP)                               
          CALL SETUP(A, IZ, W0, IE)                                     
C          CALL SIMCON(ZL, W0, PREC, NTRY, AREA1U, EAR1U, NOU, RSIM, R2N
          NF=1                                                          
          CALL QROMB(FNEW,ZL,W0,AREA1U)                                 
          CALL QROMB(XF,ZL,W0,EAR1U)                                    
C          IF (RSIM .GT. PREC) WRITE(36, 9015) '  DESIRED ',IE,         
C     1        'U PRECISION NOT REACHED',RSIM                           
C                                                                       
C-SAVE LOG(F1/F0).                                                      
C                                                                       
          DELF = ALOG10(AREA1U / AREA)                                  
          AREA = AREA1U                                                 
          EAREA = EAR1U                                                 
          IF (IPRINT .NE. 0) WRITE(36, 9013)  ' LOG(F', IE,             
     1       '/F0) = ', DELF, ' FOR BETAS, + OR -'                      
      ENDIF                                                             
      EBAR = EAREA / AREA                                               
      EBARP = EAREAP / AREAP                                            
      EBARM = EBAR                                                      
      IF (AREAM .NE. 0.) EBARM = EAREAM / AREAM                         
      DEBAR = ELMASS * 1000. *                                          
     1    AMAX1(ABS(EBAR - EBARP), ABS(EBAR - EBARM))                   
      EBAR = (EBAR - 1.) * ELMASS                                       
      EBARR = EBAR / ELMASS / (W0 - ZL)                                 
      EBAR = EBAR * 1000.                                               
C                                                                       
C-IZ < 0 FOR B- DECAY.                                                  
C-IZ = 0 IS AN ERROR. (took care before)                                
C-IZ > 0 FOR EC / B+ DECAY.                                             
C                                                                       
      IF(IZ.GT.0) THEN                                                  
C                                                                       
C-CALCULATE CAPTURE TO POSITRON RATIO.                                  
C-RESTORE FROM POSITRON ENERGY TO TOTAL DECAY ENERGY.                   
C                                                                       
         W0 = W0 + 2.                                                   
C        Must protect against floating overflows for either very small  
C          capture or positron decay (TWB. 921216)                      
         IF(FC .LE. AREA)THEN                                           
            ETOP = FC / AREA                                            
            ETOPP = FCP / AREAP                                         
            ETOPM = ETOP                                                
            IF (AREAM .NE. 0.) ETOPM = FCM / AREAM                      
C                                                                       
C-DETOP USED FOR SQUARE OF UNCERTAINTY IN EC / POSITRON RATIO.          
C                                                                       
            DETOP = (AMAX1(ABS(ETOP - ETOPP), ABS(ETOP - ETOPM))) ** 2  
C                                                                       
C-ADD A 1% UNCERTAINTY FOR THE THEORY.                                  
C                                                                       
            DETOP = DETOP + 0.0001 * ETOP * ETOP                        
            DETOP = SQRT(DETOP)                                         
         ELSE                                                           
C           Calculate positron to capture ratio instead                 
            ETOP=AREA/FC                                                
            ETOPP=AREAP/FCP                                             
            ETOPM=ETOP                                                  
            IF(FCM .NE. 0)ETOPM=AREAM/FCM                               
            DETOP = (AMAX1(ABS(ETOP - ETOPP), ABS(ETOP - ETOPM))) ** 2  
            DETOP = DETOP + 0.0001 * ETOP * ETOP                        
            DETOP = SQRT(DETOP)                                         
C           Now invert to get back to capture to positron ratio         
            DETOP=(DETOP/ETOP)/ETOP                                     
            ETOP=1.0/ETOP                                               
         ENDIF                                                          
         EKTOP = FCK / AREA                                             
         ETOPL = ALOG10(ETOP)                                           
C                                                                       
         IF  (IBONLY .EQ. 1) THEN                                       
C                                                                       
C-POSITRON INTENSITY ALONE WAS GIVEN.                                   
C                                                                       
            PINT = PERC                                                 
            EINT = PERC * ETOP                                          
            DPINT = DPERC                                               
            DEINT = EINT *                                              
     1      (DPERC * DPERC / PERC / PERC + DETOP * DETOP / ETOP / ETOP) 
            PERC = PINT + EINT                                          
            DPERC = PERC * SQRT(DPINT * DPINT / PINT / PINT +           
     1       DETOP * DETOP / (1. + ETOP) ** 2)                          
            TP = 100. * THALF(INUMP) / PERC                             
            DTP = TP *                                                  
     1       SQRT(DT(INUMP) * DT(INUMP) / THALF(INUMP) / THALF(INUMP)   
     1         + DPERC * DPERC / PERC / PERC)                           
            DFLT = DTP * 0.43429 / TP                                   
            FLOGT = ALOG10(TP)                                          
            WRITE(36, 9016)                                             
     1       'LOG OF POSITRON PLUS CAPTURE PARTIAL HALF-LIFE = ',FLOGT  
            CALL CNVU2S(PERC, DPERC, AN(3), 8, DAN(3), 2)               
            CALL CNVU2S(TP, DTP, AN(4), 12, DAN(4), 2)                  
            WRITE(36, 9010)                                             
     1       (   AN(J)(1:8), DAN(J)(1:2),                               
     2        AN(J+1),DAN(J+1)(1:2),J = 1, 3, 2)                        
         ELSE                                                           
C                                                                       
C-TOTAL INTENSITY (EC AND B+) WAS GIVEN.                                
C                                                                       
C           Cannot use ETOP+-DETOP to calculate positron intensity when 
C             electron to capture ratio is large --- DPINT possibly     
C             overestimated. Cannot use PERC-PINT when electron to      
C*            capture ratio is low --- possible machine roundoff error  
            IF(FC .LE. AREA)THEN                                        
               EINT=PERC*(ETOP/(1+ETOP))                                
               PINT = PERC / (1. + ETOP)                                
               DPINT = PINT * SQRT(DPERC * DPERC / PERC / PERC +        
     1           DETOP * DETOP / (1. + ETOP) ** 2)                      
            ELSE                                                        
               PINT=AREA/(AREA+FC)                                      
               PINTP=AREAP/(AREAP+FCP)                                  
               PINTM=AREAM/(AREAM+FCM)                                  
               DPINT=AMAX1(ABS(PINTP-PINT),ABS(PINT-PINTM))**2          
C              Account for uncertainty in theory                        
               DPINT=DPINT+(0.01*ETOP/(1.+ETOP)**2)**2                  
               DPINT=SQRT(PERC*PERC*DPINT + (PINT*DPERC)**2)            
               PINT=PERC*PINT                                           
               EINT = PERC - PINT                                       
               DEINT=(DETOP/ETOP)**2                                    
               DEINT=DEINT/((1.+ETOP)**2)                               
               DEINT=EINT*SQRT(DPERC*DPERC/PERC/PERC +DEINT)            
            ENDIF                                                       
         ENDIF                                                          
C   changes the order of this formula due to overflow                   
C             DEINT = EINT * SQRT(DPERC * DPERC / PERC / PERC +         
C     1        DETOP * DETOP / (ETOP * (1. + ETOP)) ** 2)               
C                                                                       
               DEINT=(DETOP/ETOP)**2                                    
               DEINT=DEINT/((1.+ETOP)**2)                               
               DEINT=EINT*SQRT(DPERC*DPERC/PERC/PERC +DEINT)            
C                                                                       
C        NOTE: pint and eint are now correlated                         
C                                                                       
C                                                                       
C-PRINT CAPTURE AND POSITRON RATIOS AND INTENSITIES.                    
C                                                                       
         WRITE(36, 9017) ETOP, DETOP, ETOPL, EKTOP                      
         WRITE(36, 9018) PINT, DPINT, EINT, DEINT                       
C                                                                       
C-COMBINE POSITRON AND CAPTURE.                                         
C-ALSO ENTER HERE WITH NPOS = 0 IF EC ONLY (E < 1022).                  
C                                                                       
         AREA = AREA + FC                                               
         AREAP = AREAP + FCP                                            
         AREAM = AREAM + FCM                                            
         IF (AREA .LE. 0.) THEN                                         
C                                                                       
C-ERROR EXIT FOR NEGATIVE OR ZERO INTEGRATION.                          
C                                                                       
             WRITE(36, 9000)                                            
     1          '0   ZERO OR NEGATIVE INTEGRATION. OLD CARD KEPT'       
             ISKIP1 = 1                                                 
             somskp=.TRUE.                                              
             GOTO 900                                                   
         ENDIF                                                          
      ENDIF                                                             
410   CONTINUE                                                          
C     Moved call to WECARD to here so program has access to the new     
C       E record contents (TWB. 930212)                                 
      IF (IZ .GE. 0) THEN                                               
C                                                                       
C-SKIP FOLLOWING SECTION FOR B- DECAY.                                  
C                                                                       
C-PLACE IE, IB IN PROPER FIELDS ON NEW FIRST E-CARD.                    
C                                                                       
         CALL WECARD                                                    
ctwb         IF (EK(INUMP) .LE. 1022.) GOTO 530                         
      ENDIF                                                             
C                                                                       
C-COMBINE LOG F AND LOG T FOR EITHER B- OR EC / B+.                     
C                                                                       
      FLOG = ALOG10(AREA)                                               
      FLOGP = ALOG10(AREAP)                                             
      FLOGM = FLOG                                                      
      IF (AREAM .GT. 0.) FLOGM = ALOG10(AREAM)                          
      FLOGF = FLOGT + FLOG                                              
      FT = AREA * TP                                                    
      DLAREA = AMAX1(ABS(FLOG - FLOGP), ABS(FLOG - FLOGM))              
      DLFT = SQRT(DLAREA * DLAREA + DFLT * DFLT)                        
C                                                                       
      FTOUT = FLOGF                                                     
      IF (AREAM .LE. 0.) FTOUT = FLOGT + FLOGP                          
C     Program was not always outputing proper significant digits        
C       (TWB. 930212)                                                   
      IF (ZERUNC .NE. 0                                                 
     2  .AND. ((CARD(30:30) .GE. 'A' .AND. CARD(30:30) .LE. 'Z')        
     3  .OR. (CARD(40:40) .GE. 'A' .AND. CARD(40:40) .LE. 'Z')          
     4  .OR. (CARD(30:31) .EQ. '  ' .AND. CARD(40:41) .EQ. '  '))) THEN 
C         PRINT ONLY TO FIRST DECIMAL PLACE                             
          CALL CNVU2S(FTOUT, 0.3, AN(1), 8, DAN(1), 2)                  
          DAN(1) = ' '                                                  
          IF(UNCERT.NE.' ') CALL UNCALP(DAN(1),UNCERT,1)                
      ELSE                                                              
          CALL CNVU2S(FTOUT, DLFT, AN(1), 8, DAN(1), 2)                 
      ENDIF                                                             
C                                                                       
C    blank out if number is all zero                                    
C                                                                       
      CALL ZBLANK(AN(1)(1:8),DAN(1))                                    
      CARD(42:49)=AN(1)(1:8)                                            
      CALL CENTER(CARD(42:49),8)                                        
      CARD(50:55)=' '                                                   
      CARD(50:51)=DAN(1)(1:2)                                           
      IF (AREAM .LE. 0.) CARD(50:51)='LE'                               
C                                                                       
C-RE-ENTER HERE FOR B- AS WELL AS FOR EC/B+.                            
C                                                                       
C     Changed from check on pint to check if IB was output (TWB. 930212)
      IF (C8 .EQ. 'E' .AND. card(22:29) .EQ. ' ')GOTO 530               
C                                                                       
C-PUT AVERAGE B- OR B+ ENERGY ON SECOND E-CARD.                         
C                                                                       
C     Program was assuming fractional uncertainty but using absolute    
C       (TWB. 930212)                                                   
      DEE = DEBAR/ebar                                                  
C                                                                       
C-DON'T SHOW UNCERTAINTY IF LESS THAN 0.01%.                            
C-ROUND OFF AS IF UNCERTAINTY IS 0.1 KEV.                               
C     Changed from 0.1% and rewrote algorithm (TWB. 930212)             
C                                                                       
      If(dee .le. 0.0001)Then                                           
         Call cnvu2s(ebar,0.1,an(1),11,stb,1)                           
      Else                                                              
         CALL CNVU2S(EBAR, DEBAR, AN(1), 11,STB,-2)                     
      Endif                                                             
      IF (AN(1)(1:8) .NE. STARS) THEN                                   
         Call Addstr(card2,10,'EAV=')                                   
         Call Lbsup(an(1)(1:11))                                        
         Call Addstr(card2,14,an(1)(1:Lenstr(an(1))))                   
         If(card2(14+Lenstr(an(1)):) .NE. ' ')                          
     2     Call Addstr(card2,14+Lenstr(an(1)),'$')                      
      ENDIF                                                             
  530 WRITE(36, 9020) EK(INUMP), IE, FLOG, DLAREA                       
      WRITE(36, 9021) IE, FLOGF, DLFT, IE, FT                           
      IF (NPOS .NE. 0) WRITE(36, 9022) EBAR, DEBAR, EBARR               
C                                                                       
C   Unless dit is the last parent card value, loop back to 300          
C                                                                       
      IF(INUMP.LT.NUMP) GO TO 300                                       
C                                                                       
      WRITE(36, 9023) CARDI, 'OLD CARD'                                 
      IF (ISKIP + ISKIP1 .NE. 0) THEN                                   
C                                                                       
C-IF THERE WAS AN ERROR, THEN PRINT AND PUNCH OLD CARD.                 
C                                                                       
          WRITE(36, 9024) 'OLD CARD KEPT'                               
          WRITE(21, 9000) CARDI                                         
         crdcom='*****'                                                 
          GOTO 100                                                      
      ENDIF                                                             
C                                                                       
C-IF NO ERRORS, THEN PRINT AND PUNCH TWO NEW CARDS.                     
C                                                                       
      WRITE(36, 9025) CARD, 'NEW CARD'                                  
      WRITE(21, 9000) CARD                                              
      CARD2(1:5)=CARD(1:5)                                              
      CARD2(6:6)='S'                                                    
      CARD2(8:8)=C8                                                     
      WRITE(36, 9025) CARD2, 'NEW CARD'                                 
      WRITE(21, 9000) CARD2                                             
      If(dowarn)Then                                                    
         Write(6,FMT='(1X,A)')card                                      
         Write(6,FMT='(A)')                                             
     2     '  ***** Allowed spectrum assumed for calcuations *****'     
         Write(36,FMT='(A)')                                            
     2     '  ***** Allowed spectrum assumed for calcuations *****'     
         crdcom=card(1:5)                                               
         crdcom(7:7)='c'                                                
         crdcom(8:8)=c8                                                 
         crdcom(10:)='LOGFT'                                            
         If(c8 .EQ. 'E')Then                                            
            If(card(22:29) .NE. ' ')                                    
     2        Call Addstr(crdcom,Lenstr(crdcom)+1,',IB')                
            If(card(32:39) .NE. ' ')                                    
     2        Call Addstr(crdcom,Lenstr(crdcom)+1,',IE')                
         EndIf                                                          
         Call Addstr(crdcom,Lenstr(crdcom)+1,'$')                       
         crdcom(Lenstr(crdcom)+1:)=                                     
     2     'Allowed spectrum assumed for calcuations by ^LOGFT'         
         Write(21,9000)crdcom                                           
      EndIf                                                             
      GO TO 100                                                         
C                                                                       
C-CHECK OLD SECOND CARDS.(B OR E)                                       
C           ============                                                
C                                                                       
  600 IF(C6.EQ.'S' .OR. C6.EQ.'2') THEN                                 
         IF(C79.EQ.' B' .OR. C79.EQ.' E') THEN                          
            WRITE(36, 9002) CARD                                        
C           Change column 6 to "S" after writing the card instead of    
C             before (TWB. 930212)                                      
            CARD(6:6)='S'                                               
            IF (A .EQ. 0.) THEN                                         
               WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'         
               crdcom='*****'                                           
               GOTO 900                                                 
            ENDIF                                                       
            IF (ISKIP + ISKIP1 .EQ. 0) THEN                             
C                                                                       
C-IF A SECOND CARD WAS THERE, THEN PRINT IT ALSO FOR REFERENCE.         
C                                                                       
               WRITE(36, 9004) 'CHECK OLD SECOND CARD'                  
               WRITE(36, 9000) '0'                                      
               GOTO 100                                                 
            ENDIF                                                       
         ENDIF                                                          
      Else                                                              
C        Had to allow for evaluators using other than "2" or "S"        
C          (TWB. 930212)                                                
         If(c6 .NE. ' ' .AND. c79 .EQ. ' B')Then                        
            If(Indexf(card,10,'EAV=') .GT. 0)Then                       
               WRITE(36, 9002) CARD                                     
               CARD(6:6)='S'                                            
               IF (A .EQ. 0.) THEN                                      
                  WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'      
                  crdcom='*****'                                        
                  GOTO 900                                              
               ENDIF                                                    
               IF (ISKIP + ISKIP1 .EQ. 0) THEN                          
                  WRITE(36, 9004) 'CHECK OLD SECOND CARD'               
                  WRITE(36, 9000) '0'                                   
                  GOTO 100                                              
               ENDIF                                                    
            Endif                                                       
         Endif                                                          
         If(c6 .NE. ' ' .AND. c79 .EQ. ' E')Then                        
            If(Indexf(card,10,'EAV=') .GT. 0                            
     2         .OR. Indexf(card,10,'CK=') .GT. 0                        
     3         .OR. Indexf(card,10,'CL=') .GT. 0                        
     4         .OR. Indexf(card,10,'CM+=') .GT. 0)Then                  
               WRITE(36, 9002) CARD                                     
               CARD(6:6)='S'                                            
               IF (A .EQ. 0.) THEN                                      
                  WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'      
                  crdcom='*****'                                        
                  GOTO 900                                              
               ENDIF                                                    
               IF (ISKIP + ISKIP1 .EQ. 0) THEN                          
                  WRITE(36, 9004) 'CHECK OLD SECOND CARD'               
                  WRITE(36, 9000) '0'                                   
                  GOTO 100                                              
               ENDIF                                                    
            Endif                                                       
         Endif                                                          
      ENDIF                                                             
C                                                                       
C-PRODUCE NEW DATA SET.                                                 
C                                                                       
  900 If(dowarn .AND. crdcom.NE.'*****')Then                            
         If(card .NE. crdcom)Write(21,9000)card                         
      Else                                                              
         WRITE(21, 9000) CARD                                           
      EndIf                                                             
      GOTO 100                                                          
C                                                                       
C-HERE ON END OF INPUT.                                                 
C                                                                       
 1265 Continue                                                          
      If(iskip .gt. 0)Then                                              
         Write(6,9002)'***** Data set will not be modified - '//        
     2     'See LOGFT report'                                           
      Else                                                              
         If(somskp)Write(6,9002)'***** Some records will not be '//     
     2     'modified - See LOGFT report'                                
      Endif                                                             
      WRITE(36, 9000) '0  ALL PROBLEMS COMPLETED'                       
C+++MDC+++                                                              
C...VAX                                                                 
C/      CALL EXIT                                                       
C...DVF,UNX,ANS                                                         
      STOP                                                              
C---MDC---                                                              
C                                                                       
      END                                                               
        REAL FUNCTION XF(X)                                             
C                                                                       
C  Used to calculate the average B+- energies                           
C                                                                       
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************
C*                           Present Code                               
C*                           ------------                               
C*  FNEW                                                                
C*                                                                      
C***********************************************************************
        REAL X                                                          
C                                                                       
        REAL FNEW                                                       
        EXTERNAL FNEW                                                   
C                                                                       
        REAL Y                                                          
C                                                                       
        Y=FNEW(X)                                                       
        IF(X .NE. 0. .AND. Y .NE. 0.)THEN                               
           XF=X*Y                                                       
        ELSE                                                            
           XF=0.                                                        
        ENDIF                                                           
        RETURN                                                          
        END                                                             
C-----  Integration Routines                                            
        SUBROUTINE QROMB(FUNC,A,B,SS)                                   
C  Returns as SS the integral of the function FUNC FROM A TO B.         
C    Integration is performed by Romberg's method of order 2K,          
C    where, e.g., K=2 is Simpson's Rule                                 
C    [Numerical Recipes (The Art of Scientific Computing).  W.H. Press, 
C    et al.  Cambridge University Press (NY, 1986), p.114]              
C                                                                       
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************
C*                           Present Code                               
C*                           ------------                               
C*  POLINT  TRAPZD                                                      
C*                                                                      
C*                        FORTRAN 77 Supplied                           
C*                        -------------------                           
C*  ABS                                                                 
C*                                                                      
C***********************************************************************
        REAL FUNC,A,B,SS                                                
        EXTERNAL FUNC                                                   
C                                                                       
        INTEGER JMAX,JMAXP,K,KM                                         
C        PARAMETER (JMAX=30,JMAXP=JMAX+1,K=3,KM=K-1)                    
        PARAMETER (JMAX=20,JMAXP=JMAX+1,K=3,KM=K-1)                     
C                                                                       
        REAL EPS,DUM                                                    
        COMMON/PREC1/EPS,DUM                                            
C                                                                       
        REAL S(JMAXP),H(JMAXP),DSS                                      
        INTEGER J,JJ                                                    
C                                                                       
        REAL ABS,ALOG10                                                 
        INTRINSIC ABS,ALOG10,DABS,DBLE,INT                              
        Save
C                                                                       
      SS=0.                                                             
      DSS=0.                                                            
        H(1)=1.                                                         
        DO 100 J=1,JMAX                                                 
           JJ=J                                                         
           CALL TRAPZD(FUNC,A,B,S(J),JJ)                                
 
           IF(J .GE. K)THEN                                             
              CALL POLINT(H(J-KM),S(J-KM),K,0.,SS,DSS)                  
C       Add check for when integral is zero                             
              IF(SS .EQ. 0. .AND. DSS .EQ. 0.)RETURN                    
C              IF(ABS(DSS) .LE. EPS*ABS(SS))RETURN                      
              IF(DABS(DBLE(DSS)/DBLE(SS)) .LE. EPS)RETURN               
           ENDIF                                                        
           S(J+1)=S(J)                                                  
           H(J+1)=0.25*H(J)                                             
100     CONTINUE                                                        
C        PAUSE 'TOO MANY STEPS --- QROMB'
          write(*,*) 'Too many steps --- QROMB'
          read(*,*)                                
        END                                                             
C                                                                       
        SUBROUTINE TRAPZD(FUNC,A,B,S,N)                                 
C  Compute's the Nth stage of refinement of an extended trapoziodal rule
C    FUNC is input as the name of the function to be integrated between 
C    limits A and B, also input.  S should not be modified between      
C    sequential calls.                                                  
C    [Numerical Recipes (The Art of Scientific Computing).  W.H. Press, 
C    et al.  Cambridge University Press (NY, 1986), p.111]              
C                                                                       
C  Changed to double precision where necessary to avoid convergence     
C    problems (TWB. 930212)                                             
C                                                                       
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************
C*                           Present Code                               
C*                           ------------                               
C*  FUNC*                                                               
C*                                                                      
C*  *Dummy routine                                                      
C*                                                                      
C***********************************************************************
        INTEGER N                                                       
        REAL FUNC,A,B,S                                                 
        EXTERNAL FUNC                                                   
C                                                                       
        INTEGER IT,J                                                    
      DOUBLE PRECISION TNM,DEL,SUM,X                                    
C                                                                       
      REAL REAL                                                         
      DOUBLE PRECISION DBLE                                             
      INTRINSIC DBLE,REAL                                               
      Save
C                                                                       
        IF(N .EQ. 1)THEN                                                
           S=0.5*(B-A)*(FUNC(A)+FUNC(B))                                
           IT=1                                                         
        ELSE                                                            
           TNM=DBLE(IT)                                                 
           DEL=(DBLE(B)-DBLE(A))/TNM                                    
           X=A+0.5D+0*DEL                                               
           SUM=0.D+0                                                    
           DO 100 J=1,IT                                                
              SUM=SUM+DBLE(FUNC(REAL(X)))                               
              X=X+DEL                                                   
100        CONTINUE                                                     
           S=0.5D+0*(S+(DBLE(B)-DBLE(A))*SUM/TNM)                       
           IT=2*IT                                                      
        ENDIF                                                           
        RETURN                                                          
        END                                                             
        SUBROUTINE POLINT(XA,YA,N,X,Y,DY)                               
C  Given arrays XA and YA, each of length N, and given a value X, THE   
C    value Y and an error estimate DY are returned                      
C    [Numerical Recipes (The Art of Scientific Computing.  W.H. Press,  
C    et al.  Cambridge University Press (NY, 1986), p.82]               
C                                                                       
        INTEGER N                                                       
        REAL XA(N),YA(N),X,Y,DY                                         
C                                                                       
        INTEGER NMAX                                                    
        PARAMETER (NMAX=10)                                             
C                                                                       
        INTEGER NS,I,M                                                  
        REAL C(NMAX),D(NMAX),DIF,DIFT,HO,HP,DEN,W                       
C                                                                       
      REAL SCALE,MAXVAL,MINVAL                                          
      INTEGER TEST                                                      
C                                                                       
      REAL ABS,ALOG10,AMAX1,AMIN1                                       
      INTEGER INT                                                       
      INTRINSIC ABS,ALOG10,AMAX1,AMIN1,INT                              
      Save
                                                                        
C                                                                       
        IF(N .LE. 1 .OR. N .GT. NMAX)THEN                               
           DY=10000.                                                    
           RETURN                                                       
        ENDIF                                                           
        NS=1                                                            
        DIF=ABS(X-XA(1))                                                
      MAXVAL=0.                                                         
      MINVAL=0.                                                         
      SCALE=1.                                                          
        DO 100 I=1,N                                                    
           DIFT=ABS(X-XA(I))                                            
           IF(DIFT .LT. DIF)THEN                                        
              NS=I                                                      
              DIF=DIFT                                                  
           ENDIF                                                        
           C(I)=YA(I)                                                   
           D(I)=YA(I)                                                   
           IF(YA(I) .NE. 0.)THEN                                        
              MAXVAL=AMAX1(MAXVAL,ALOG10(ABS(YA(I))))                   
              MINVAL=AMIN1(MINVAL,ALOG10(ABS(YA(I))))                   
           ENDIF                                                        
100     CONTINUE                                                        
        Y=YA(NS)                                                        
C     Try to keep the scale within reasonable limits                    
      TEST=INT(MINVAL)                                                  
      IF(TEST .LT. -5)SCALE=10.**(-TEST)                                
      TEST=INT(MAXVAL)                                                  
      IF(TEST .GT. 5)SCALE=SCALE*10.**(-TEST)                           
      IF(SCALE .NE. 1.)THEN                                             
         Y=Y*SCALE                                                      
         DO 120 I=1,N                                                   
            C(I)=SCALE*C(I)                                             
            D(I)=SCALE*D(I)                                             
120      CONTINUE                                                       
      ENDIF                                                             
        NS=NS-1                                                         
        DO 200 M=1,N-1                                                  
           DO 150 I=1,N-M                                               
              HO=XA(I)-X                                                
              HP=XA(I+M)-X                                              
              W=C(I+1)-D(I)                                             
              DEN=HO-HP                                                 
              IF(DEN .EQ. 0.)THEN                                       
                 DY=1000.*Y                                             
                 IF(SCALE .NE. 1.)THEN                                  
                    Y=Y/SCALE                                           
                    DY=DY/SCALE                                         
                 ENDIF                                                  
                 RETURN                                                 
              ENDIF                                                     
              DEN=W/DEN                                                 
              D(I)=HP*DEN                                               
              C(I)=HO*DEN                                               
150        CONTINUE                                                     
           IF(2*NS .LT. N-M)THEN                                        
              DY=C(NS+1)                                                
           ELSE                                                         
              DY=D(NS)                                                  
              NS=NS-1                                                   
           ENDIF                                                        
           Y=Y+DY                                                       
200     CONTINUE                                                        
      IF(SCALE .NE. 1)THEN                                              
         Y=Y/SCALE                                                      
         DY=DY/SCALE                                                    
      ENDIF                                                             
        RETURN                                                          
        END                                                             
      SUBROUTINE SETUP(A, IZ, W0, IE)                                   
C                                                                       
C  ASSOCIATED WITH FUNCTION F TO REDUCE THE NUMBER OF OPERATIONS IN F.  
C  SETUP MUST BE CALLED BEFORE F.                                       
C                                                                       
      REAL A, W0                                                        
      INTEGER IZ, IE                                                    
C                                                                       
      REAL APOW, BPOW, CALPHA, CM50A, CM50B,                            
     1    CM80A, CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, 
     2    G1, G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2, TWOG3, TWOR, V, 
     3    W0X                                                           
      COMPLEX BQ, BQU                                                   
      INTEGER IEX, IFIN, IZX                                            
      COMMON /FCOM/ APOW, BPOW, BQ, BQU, CALPHA, CM50A, CM50B, CM80A,   
     1    CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, G1,    
     2    G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2,                     
     3    TWOG3, TWOR, V, W0X, IEX, IFIN, IZX                           
C                                                                       
      REAL ALPHA, ATEMP, C, CALPHS, CM50, CM80,                         
     1    CUBRTA, GI, GR, GRSQ, PI, RSQ, RSQSQ, TAZR, TGM1,             
     2    TWOPI, ZR, ZR2, ZR3                                           
      INTEGER IZTEMP                                                    
C                                                                       
      DATA ALPHA/7.2973E-3/                                             
      DATA PI, TWOPI/3.14 159 265 359, 6.28 318 530 718/                
      DATA ATEMP, IZTEMP/0., 0/                                         
      Save
C                                                                       
      IZX = IZ                                                          
      W0X = W0                                                          
      IEX = IE                                                          
C                                                                       
      IF ((A .EQ. ATEMP) .AND. (IZ .EQ. IZTEMP)) GOTO 200               
C-CALCULATE NUCLEAR RADIUS (UNLESS ALREADY DONE).                       
      IF (A .EQ. ATEMP) GOTO 190                                        
          ATEMP = A                                                     
          CUBRTA = ATEMP ** 0.33 333 333 333                            
          R = 2.908E-3 * CUBRTA - 2.437E-3 / CUBRTA                     
          RSQ = R * R                                                   
          TWOR = R + R                                                  
  190 IF (IZ .EQ. IZTEMP) GOTO 240                                      
          IZTEMP = IZ                                                   
          C = IZTEMP                                                    
          C = ABS(C)                                                    
C-CALCULATE TERMS FOR SCREENING (UNLESS ALREADY DONE).                  
          V = ((-9.45E-9 * C + 3.014E-6) * C + 1.881E-4) * C - 5.166E-4 
          IF (IZTEMP .LT. 0) GOTO 230                                   
              APOW = ((1.11E-7 * C - 1.01E-5) * C - 2.38E-3) * C + 0.102
              BPOW = ((-2.42E-8 * C + 3.83E-6) * C + 3.60E-5) * C-0.0156
C-FIRST WE COMPUTE THE PART OF THE INTEGRAND THAT IS CONSTANT           
C-AND CAN BE PUT IN FRONT OF THE INTEGRAL SIGN.                         
C-1/2 R**2 GAMMASQ(2G + 1).                                             
  230     CALPHA = C * ALPHA                                            
          CALPHS = CALPHA * CALPHA                                      
          G1 = SQRT(1. - CALPHS)                                        
          TWOG = G1 + G1                                                
          ZR = TWOG + 1.                                                
          G1P1 = G1 + 1.                                                
          TGM1 = TWOG - 1.                                              
          CALL GAMMA(ZR, 0., GR, GI)                                    
          GRSQ = GR * GR                                                
          BQ = CMPLX(ZR, 0.)                                            
  240 FRONT = 0.5 / RSQ / GRSQ                                          
      IFIN = 0                                                          
      IF (IZTEMP .GT. 0) GOTO 75                                        
C-FOR NEGATRONS, SET UP CONSTANT FOR LOW-E APPROXIMATION.               
          TAZR = TWOR * CALPHA                                          
          FRLOW = TWOPI * TWOR * (TAZR ** TGM1) * G1P1 *                
     1        (1. - TAZR * 3. / ZR)                                     
C-FOR LARGE Z NEGATRONS, SET UP SCREENING CONSTANTS.                    
          IF (IZTEMP .GE. -50) GOTO 75                                  
              CM50 = C - 50.                                            
              CM50A = -2.5E-3 * CM50 + 1.                               
              CM50B = -4.0E-6 * CM50 * CM50                             
              IFIN = -1                                                 
              GOTO 200                                                  
C-LARGE Z POSITRON SCREENING CONSTANTS.                                 
   75 IF (IZTEMP .LE. 80) GOTO 200                                      
          CM80 = C - 80.                                                
          CM80A = -1.7E-4 * CM80                                        
          CM80B = +6.3E-4 * CM80                                        
          CM80C = -8.8E-3 * CM80                                        
          IFIN = 1                                                      
  200 IF (IE .LT. 1) GOTO 99                                            
C-SET UP FOR FIRST FORBIDDEN UNIQUE.                                    
      G2 = SQRT(4. - CALPHS)                                            
      TWOG2 = G2 + G2                                                   
      ZR2 = TWOG2 + 1.                                                  
      G2P1 = G2 + 1.                                                    
      CALL GAMMA(ZR2, 0., GR, GI)                                       
      BQU = CMPLX(ZR2, 0.)                                              
      RSQSQ = RSQ * RSQ                                                 
      CONU = 4.5 / RSQSQ / GR / GR                                      
      IF (IZTEMP .GT. 0) GOTO 250                                       
C-SET UP CONSTANT FOR LOW-E NEGATRON FIRST FORBIDDEN DECAY.             
          CONLU = 72. * PI * CALPHA * (1. + G2P1) *                     
     1        (TAZR ** (ZR2 - 3.)) * (1. - 2.5 * TAZR / ZR2) /          
     2        RSQ / GR / GR                                             
  250 IF (IE .LE. 1) GOTO 99                                            
C-SET UP FOR SECOND FORBIDDEN UNIQUE.                                   
      G3 = SQRT(9. - CALPHS)                                            
      TWOG3 = G3 + G3                                                   
      ZR3 = TWOG3 + 1.                                                  
      G3P1 = G3 + 1.                                                    
      CALL GAMMA(ZR3, 0., GR, GI)                                       
      BQU = CMPLX(ZR3, 0.)                                              
      CONU = CONU * 3.33 333 333 333                                    
      CON2U = 112.5 / RSQSQ / GR / GR / RSQ                             
      IF (IZTEMP .GT. 0) GOTO 99                                        
          CONLU = CONLU * 3.33 333 333 333                              
          COL2U = 270. * PI * (TAZR ** (ZR3 - 3.)) *                    
     1        (G3P1 + 2.) * CALPHA * (1. - 2.33 333 333 * TAZR /        
     2        ZR3) / RSQSQ / GR / GR                                    
   99 RETURN                                                            
      END                                                               
      REAL FUNCTION F(W)                                                
C                                                                       
C  Fermi function used in calculating B+- spectra and associated        
C    internal Bremsstrahlung.  Modified to be more consistent with F77  
C    procedures                                                         
C                                                                       
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************
C*                           Present Code                               
C*                           ------------                               
C*  GAMMA                                                               
C*                           NSDMTH  1(00)                              
C*                           -------------                              
C*  HYPERG                                                              
C*                                                                      
C*                        FORTRAN 77 Supplied                           
C*                        -------------------                           
C*  CEXP    CSQRT   EXP     SQRT                                        
C*                                                                      
C***********************************************************************
      REAL W                                                            
C                                                                       
      REAL APOW, BPOW, CALPHA, CM50A, CM50B,                            
     1    CM80A, CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, 
     2    G1, G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2, TWOG3, TWOR, V, 
     3    W0                                                            
      COMPLEX BQ, BQU                                                   
      INTEGER IE, IFIN, IZ                                              
      COMMON /FCOM/ APOW, BPOW, BQ, BQU, CALPHA, CM50A, CM50B, CM80A,   
     1    CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, G1,    
     2    G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2,                     
     3    TWOG3, TWOR, V, W0, IE, IFIN, IZ                              
C                                                                       
      REAL B, BA, F1U, F2U,                                             
     1    GI, GR, P, PI, PSQ, TWORP,                                    
     2    W0MW, W0MWSQ, X, XSQ, Y, YOW                                  
      COMPLEX AQ, B1C, B2C, F1C, G1C, G1Y, G2Y, HGF, HYPERG, QC, XQ     
C                                                                       
      REAL EXP,SQRT                                                     
      COMPLEX CEXP,CMPLX,CSQRT                                          
      INTRINSIC CEXP,CMPLX,CSQRT,EXP,SQRT                               
C                                                                       
      DATA PI/3.14 159 265 359/                                         
      Save
C                                                                       
C  Neutrino energy is not screened, so calculate it first.              
      W0MW = W0 - W                                                     
C  At BETA endpoint function is zero.                                   
      IF (W0MW .LE. 0.)THEN                                             
         F = 0.                                                         
         RETURN                                                         
      ENDIF                                                             
      W0MWSQ = W0MW * W0MW                                              
C  For B-, we simply shift the energy down by V.                        
      IF (IZ .LE. 0)THEN                                                
         X = W - V                                                      
C  For low-E B-, we use a power series approximation.                   
         IF (X .LE. 1.0001)THEN                                         
            F = FRLOW * X                                               
            GOTO 50                                                     
         ENDIF                                                          
      ELSE                                                              
C  For very low-E B+, value is too small to contribute to integral.     
         IF (W .LT. 1.001)THEN                                          
            F=0.                                                        
            RETURN                                                      
         ENDIF                                                          
C  B+ screening.                                                        
         PSQ = W * W - 1.                                               
         P = SQRT(PSQ)                                                  
         X = W + V * EXP(APOW / P + BPOW / PSQ)                         
      ENDIF                                                             
C-CALCULATE SCREENED MOMENTUM.                                          
      XSQ = X * X                                                       
      PSQ = XSQ - 1.                                                    
      P = SQRT(PSQ)                                                     
      Y = X * CALPHA / P                                                
C  The GAMMA function code we use is not recommended above 10.          
C    So go back to the low E approximation.                             
      IF (Y .GT. 10.)THEN                                               
         IF (IZ .GT. 0)THEN                                             
            F=0.                                                        
            RETURN                                                      
         ENDIF                                                          
         F=FRLOW*X                                                      
         GOTO 50                                                        
      ENDIF                                                             
C  Y must have negative sign for positrons.                             
      IF (IZ .GT. 0) Y = -Y                                             
C  Begin calculation of electron wave function.                         
C    Call for complex gamma function.                                   
      CALL GAMMA(G1, Y, GR, GI)                                         
      BA = EXP(PI * Y) / P                                              
C  B is that portion of the integrand common to both F and G parts      
C   of the wave function.                                               
      TWORP = TWOR * P                                                  
      B = (TWORP ** TWOG) * BA * (GR * GR + GI * GI)                    
C  Call for Hypergeometric function.                                    
      XQ = CMPLX(0., TWORP)                                             
      AQ = CMPLX(G1P1, Y)                                               
      HGF = HYPERG(AQ, BQ, XQ)                                          
      YOW = Y / X                                                       
      QC = CEXP(CMPLX(0., -P * R))                                      
      G1Y = CMPLX(G1, Y)                                                
      B1C = QC * G1Y * HGF                                              
      F1C = B1C * CSQRT(CMPLX(-1., YOW) / G1Y)                          
      G1C = B1C * CSQRT(CMPLX(1., YOW) / G1Y)                           
      F = B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +                     
     1    (X + 1.) * REAL(G1C) * REAL(G1C))                             
   50 F = F * W0MWSQ                                                    
C  Correction for finite nuclear size effect.                           
      IF (IFIN .LT. 0) F = F * (CM50A + X * CM50B)                      
      IF (IFIN .GT. 0) F = F * (1. + CM80A*X + CM80B/X + CM80C/XSQ)     
      F = F * FRONT                                                     
C  For allowed transitions we are finished here.                        
      IF (IE .LT. 1)RETURN                                              
C  First forbidden unique.                                              
C    Check for low-E FFU BETA -.                                        
      IF ((X .LE. 1.0001) .OR. (Y .GT. 10.))THEN                        
         F1U = CONLU * X                                                
      ELSE                                                              
         CALL GAMMA(G2, Y, GR, GI)                                      
         B = (TWORP ** TWOG2) * BA * (GR * GR + GI * GI)                
         AQ = CMPLX(G2P1, Y)                                            
         HGF = HYPERG(AQ, BQU, XQ)                                      
         G2Y = CMPLX(G2, Y)                                             
         B2C = QC * G2Y * HGF                                           
         F1C = B2C * CSQRT(CMPLX(-2., YOW) / G2Y)                       
         G1C = B2C * CSQRT(CMPLX(2., YOW) / G2Y)                        
         F1U = CONU * B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +         
     1     (X + 1.) * REAL(G1C) * REAL(G1C))                            
      ENDIF                                                             
      F = W0MWSQ * (F + F1U)                                            
      IF (IE .LE. 1)RETURN                                              
C  Second forbidden unique.                                             
      IF ((X .LE. 1.0001) .OR. (Y .GT. 10.))THEN                        
         F2U = COL2U * X                                                
      ELSE                                                              
         CALL GAMMA(G3, Y, GR, GI)                                      
         B = (TWORP ** TWOG3) * BA * (GR * GR + GI * GI)                
         AQ = CMPLX(G3P1, Y)                                            
         HGF = HYPERG(AQ, BQU, XQ)                                      
         G2Y = CMPLX(G3, Y)                                             
         B2C = QC * G2Y * HGF                                           
         F1C = B2C * CSQRT(CMPLX(-3., YOW) / G2Y)                       
         G1C = B2C * CSQRT(CMPLX(3., YOW) / G2Y)                        
         F2U = CON2U * B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +        
     1     (X + 1.) * REAL(G1C) * REAL(G1C))                            
      ENDIF                                                             
      F = W0MWSQ * (F + F2U)                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE GAMMA(XR, XI, ZR, ZI)                                  
      REAL XR, XI, ZR, ZI                                               
      COMPLEX X, Z, GAMA                                                
      X = CMPLX(XR, XI)                                                 
      Z = GAMA(X)                                                       
      ZR = REAL(Z)                                                      
      ZI = AIMAG(Z)                                                     
      RETURN                                                            
      END                                                               
      REAL FUNCTION CAPTUR                                              
     1    (IZ, W0, IPRINT, ITYPE, FCK, FCL, FCMNO)                      
C                                                                       
      INTEGER IZ, IPRINT, ITYPE                                         
      REAL W0, FCK, FCL, FCMNO                                          
C                                                                       
      REAL BKL(127), BL1L(128), BL2L(128), BL3L(128)                    
      REAL WM1L(128), WM3L(128)                                         
C                                                                       
      REAL BK, BL1, BL2, BL3                                            
C      REAL EKTOE, EKTOEU, ELTKU, ELTOEK, EMNOTL, EMNTLU                
      REAL ELTOT, ELTOTU                                                
      REAL FL2SQ                                                        
      REAL GL1SQ, GL3R9                                                 
      REAL PCAP, PCAPU, PKEX, PKU, PL1EX, PL1U, PL2, PL2U,              
     1    PL3U, PM3, PMN, PMNU, PN3, PSIKSQ                             
C      REAL R2TL1, R2TL1U, R3TL1U,                                      
      REAL RM3, RMNO, RN3                                               
      REAL WE, WEK, WEKSQ, WEL1, WEL1SQ, WEL2, WEL2SQ,                  
     1    WEL3, WEL3SQ, WEM3, WEM3SQ, WEMNO, WEMSQ, WEN3, WEN3SQ,       
     2    WK, WL1, WL2, WL3                                             
      REAL Z                                                            
C                                                                       
      INTEGER IZEFF                                                     
C                                                                       
C     Corrections made for Z<4.  Values taken from Appendix III of Table
C       of Isotopes, Seventh Edition.  880413                           
      DATA  BKL/ 0.014E-3,0.0246E-3,0.0000548,0.000112,0.000188,        
     1 0.000284,0.0004,                                                 
     1 0.000532,0.00068,                                                
     1 .0008669,.0010721,.0013050,.0015596,.0018389,.0021455,.0024720,  
     1 .0028224,.0032029,.0036074,.0040381,.0044928,.0049664,.0054651,  
     1 .0059892,.0065390,.0071120,.0077089,.0083328,.0089789,.0096586,  
     1 .0103671,.0111031,.0118667,.0126578,.0134737,.0143256,.0151997,  
     1 .0161046,.0170384,.0179976,.0189856,.0199995,.0210440,.0221172,  
     1 .0232199,.0243503,.0255140,.0267112,.0279399,.0292001,.0304912,  
     1 .0318138,.0331694,.0345614,.0359846,.0374406,.0389246,.0404430,  
     1 .0419906,.0435689,.0451840,.0468342,.0485190,.0502391,.0519957,  
     1 .0537885,.0556177,.0574855,.0593896,.0613323,.0633138,.0653508,  
     1 .0674164,.0695250,.0716764,.0738708,.0761110,.0783948,.0807249,  
     1 .0831023,.0855304,.0880045,.0905259,.0931050,.0957299,.098404 ,  
     1 .101137 ,.1039219,.1067553,.1096509,.1126014,.1156061,.118678 ,  
     1 .121818 ,.125027 ,.128220 ,.131590 ,.135960 ,.139490 ,.143090 ,  
     1.146780,.15054, 25 * .160/                                        
C     Values added for Z=3 and 4 from same source.  880413              
      DATA  BL1L/ 2 * 0.0,0.001E-3,0.003E-3,5 * 0.,                     
     1 .000045 ,.0000633,.0000894,.0001177,.0001487,.0001893,.0002292,  
     1 .0002702,.000320 ,.0003771,.0004378,.0005004,.0005637,.0006282,  
     1 .0006946,.0007690,.0008461,.0009256,.0010081,.0010966,.0011936,  
     1 .0012977,.0014143,.0015265,.0016539,.0017820,.0019210,.0020651,  
     1 .0022163,.0023725,.0025316,.0026977,.0028655,.0030425,.0032240,  
     1 .0034119,.0036043,.0038058,.0040180,.0042375,.0044647,.0046983,  
     1 .0049392,.0051881,.0054528,.0057143,.0059888,.0062663,.0065488,  
     1 .0068348,.0071260,.0074279,.0077368,.0080520,.0083756,.0087080,  
     1 .0090458,.0093942,.0097513,.0101157,.0104864,.0108704,.0112707,  
     1 .0116815,.0120998,.0125267,.0129680,.0134185,.0138799,.0143528,  
     1 .0148393,.0153467,.0158608,.0163875,.0169393,.017493 ,.018049 ,  
     1 .018639 ,.0192367,.019840 ,.0204721,.0211046,.0217574,.0224268,  
     1 .0230972,.0237729,.024460 ,.025275 ,.026110 ,.026900 ,.027700 ,  
     1 .028530 ,.029380 ,.030240 , 25 * 0.031/                          
      DATA  BL2L/ 9 * 0.,                                               
     1 .0000183,.0000311,.0000514,.0000731,.0000992,.0001322,.0001648,  
     1 .0002016,.0002473,.0002963,.0003500,.0004067,.0004615,.0005205,  
     1 .0005837,.0006514,.0007211,.0007936,.0008719,.0009510,.0010428,  
     1 .0011423,.0012478,.0013586,.0014762,.0015960,.0017272,.0018639,  
     1 .0020068,.0021555,.0023067,.0024647,.0026251,.0027932,.0029669,  
     1 .0031461,.0033303,.0035237,.0037270,.0039380,.0041561,.0043804,  
     1 .0046120,.0048521,.0051037,.0053594,.0056236,.0058906,.0061642,  
     1 .0064404,.0067215,.0070128,.0073118,.0076171,.0079303,.0082516,  
     1 .0085806,.0089178,.0092643,.0096169,.0099782,.0103486,.0107394,  
     1 .0111361,.0115440,.0119587,.0123850,.0128241,.0132726,.0137336,  
     1 .0142087,.0146979,.0152000,.0157111,.0162443,.0167847,.0173371,  
     1 .0179065,.0184843,.0190832,.0196932,.0203137,.0209476,.0216005,  
     1 .0222662,.0229440,.023779 ,.024385 ,.025250 ,.026020 ,.026810 ,  
     1 .027610 ,.028440 ,.029280 , 25 * 0.030/                          
      DATA  BL3L/ 9 * 0.,                                               
     1 .0000183,.0000311,.0000514,.0000731,.0000992,.0001322,.0001648,  
     1 .0002000,.0002452,.0002936,.0003464,.0004022,.0004555,.0005129,  
     1 .0005745,.0006403,.0007081,.0007786,.0008547,.0009311,.0010197,  
     1 .0011154,.0012167,.0013231,.0014358,.0015499,.0016749,.0018044,  
     1 .0019396,.0020800,.0022223,.0023705,.0025202,.0026769,.0028379,  
     1 .0030038,.0031733,.0033511,.0035375,.0037301,.0039288,.0041322,  
     1 .0043414,.0045571,.0047822,.0050119,.0052470,.0054827,.0057234,  
     1 .0059643,.0062079,.0064593,.0067162,.0069769,.0072428,.0075140,  
     1 .0077901,.0080711,.0083579,.0086480,.0089436,.0092441,.0095607,  
     1 .0098811,.0102068,.0105353,.0108709,.0112152,.0115637,.0119187,  
     1 .0122839,.0126575,.0130352,.0134186,.0138138,.0142135,.0146194,  
     1 .0150312,.0154444,.0158710,.0163003,.0167331,.0171663,.0176100,  
     1 .0180568,.0185041,.018930 ,.019452 ,.019930 ,.020410 ,.020900 ,  
     1 .021390 ,.021880 ,.022360 , 25 * 0.025/                          
      DATA WM1L/9*1.00,                                                 
     1 1.000000,1.000000,1.000000,1.000000,1.000000,0.999992,0.999980,  
     1 0.999966,0.999950,0.999934,0.999914,0.999895,0.999882,0.999870,  
     1 0.999855,0.999836,0.999818,0.999803,0.999781,0.999766,0.999734,  
     1 0.999691,0.999648,0.999602,0.999547,0.999498,0.999436,0.999370,  
     1 0.999300,0.999230,0.999158,0.999083,0.999013,0.998933,0.998855,  
     1 0.998773,0.998689,0.998596,0.998493,0.998384,0.998270,0.998153,  
     1 0.998031,0.997902,0.997759,0.997618,0.997470,0.997336,0.997193,  
     1 0.997043,0.996917,0.996781,0.996629,0.996477,0.996319,0.996150,  
     1 0.995995,0.995835,0.995682,0.995486,0.995307,0.995125,0.994910,  
     1 0.994701,0.994482,0.994263,0.994034,0.993789,0.993550,0.993298,  
     1 0.993030,0.992751,0.992464,0.992174,0.991880,0.991552,0.991229,  
     1 0.990896,0.990564,0.990211,0.989859,0.989497,0.989143,0.988800,  
     1 0.988390,0.988023,0.987695,0.987170,0.986783,0.986346,0.985900,  
     1 0.985438,0.984980,0.984540,25*0.983/                             
      DATA WM3L/9*1.00,                                                 
     1 1.000000,1.000000,1.000000,1.000000,1.000000,1.000000,0.999997,  
     1 0.999987,0.999976,0.999965,0.999950,0.999937,0.999932,0.999926,  
     1 0.999917,0.999905,0.999894,0.999884,0.999867,0.999856,0.999831,  
     1 0.999799,0.999764,0.999725,0.999683,0.999645,0.999582,0.999533,  
     1 0.999473,0.999412,0.999353,0.999290,0.999232,0.999168,0.999099,  
     1 0.999029,0.998960,0.998882,0.998794,0.998700,0.998602,0.998502,  
     1 0.998398,0.998288,0.998166,0.998048,0.997921,0.997802,0.997680,  
     1 0.997569,0.997461,0.997345,0.997222,0.997103,0.996978,0.996847,  
     1 0.996721,0.996593,0.996454,0.996312,0.996184,0.996040,0.995876,  
     1 0.995706,0.995536,0.995367,0.995191,0.995008,0.994823,0.994632,  
     1 0.994428,0.994214,0.993999,0.993783,0.993538,0.993295,0.993076,  
     1 0.992832,0.992580,0.992350,0.992082,0.991832,0.991578,0.991322,  
     1 0.991083,0.990867,0.990613,0.990260,0.990002,0.989722,0.989438,  
     1 0.989147,0.988869,0.988826,25*0.99/                              
      Save
C                                                                       
C-TO CALCULATE THE ELECTRON CAPTURE CONTRIBUTION TO THE LOG FT.         
C                                                                       
C-CAPTUR MUST BE CALLED FOR ALLOWED CASE BEFORE CALLING FOR UNIQUE.     
C                                                                       
C-WE USE BINDING ENERGIES OF DAUGHTER, WAVE FUNCTIONS OF PARENT.        
C                                                                       
      IZEFF = IZ + 1                                                    
      WE = W0 - 2.                                                      
      BK = BKL(IZ)                                                      
      BL1 = BL1L(IZ)                                                    
      BL2 = BL2L(IZ)                                                    
      BL3 = BL3L(IZ)                                                    
      WK = 1. - BK / 0.511                                              
      WL1 = 1. - BL1 / 0.511                                            
      WL2 = 1. - BL2 / 0.511                                            
      WL3 = 1. - BL3 / 0.511                                            
      WEK = WE + WK                                                     
      WEKSQ = WEK * WEK                                                 
      WEL1  = WE + WL1                                                  
      WEL1SQ= WEL1 * WEL1                                               
      WEL2  = WE + WL2                                                  
      WEL2SQ= WEL2 * WEL2                                               
      WEMNO = WE + WM1L(IZ)                                             
      WEMSQ = WEMNO * WEMNO                                             
C                                                                       
C-GET WAVE FUNCTIONS.                                                   
C                                                                       
      CALL WAVEFN(IZEFF, PSIKSQ, GL1SQ, FL2SQ, GL3R9, RMNO)             
C                                                                       
C-K-CAPTURE PORTION.                                                    
C                                                                       
C-TEST IF ENOUGH ENERGY FOR K-CAPTURE.                                  
C                                                                       
      PKEX = 0.                                                         
      IF (WEK .GT. 0.) PKEX = WEKSQ * PSIKSQ                            
C                                                                       
C-L-CAPTURE PORTION.                                                    
C                                                                       
      PL1EX = 0.                                                        
      IF (WEL1 .GT. 0.) PL1EX = WEL1SQ * GL1SQ                          
      PL2 = 0.                                                          
      IF (WEL2 .GT. 0.) PL2 = WEL2SQ * FL2SQ                            
C                                                                       
      IF (WEMNO .LE. 0.) GOTO 500                                       
      PMN = WEMSQ * RMNO                                                
C                                                                       
C-BRANCH IF UNIQUE TRANSITION.                                          
C                                                                       
      IF (ITYPE .NE. 0) GOTO 360                                        
C                                                                       
C-SUMMARIZE CAPTURE COMPONENTS.                                         
C                                                                       
          ELTOT = PL1EX + PL2                                           
          PCAP = PKEX + ELTOT + PMN                                     
          CAPTUR = 1.5708 * PCAP                                        
          FCK = 1.5708 * PKEX                                           
          FCL = 1.5708 * ELTOT                                          
          FCMNO = 1.5708 * PMN                                          
C                                                                       
C-PRINTING (OPTIONAL).                                                  
C                                                                       
          IF (IPRINT .LE. 0) GOTO 99                                    
          IF (IPRINT .LT. 2) GOTO 285                                   
              WRITE(36, 6000) IZ, BK, BL1, BL2, BL3                     
 6000         FORMAT(' BINDING ENERGIES (DAUGHTER)',                    
     1            5X, 'Z=', I5, 10X, 'K', F8.4, 10X, 'L1', F8.4, 10X,   
     2            'L2', F8.4, 10X, 'L3', F8.4)                          
              WRITE(36, 6010) IZEFF, PSIKSQ, GL1SQ, FL2SQ, GL3R9        
 6010         FORMAT(' WAVE FUNCTIONS, PARENT.  Z=', I4, 8X, 'K',       
     1            E10.2, 8X, 'L1', E10.2, 8X, 'L2', E10.2, 8X,          
     2            '9*L3/R*R', E10.2)                                    
  285     IF (PKEX .GT. 0.) GOTO 320                                    
              WRITE(36, 6020)                                           
 6020         FORMAT(20X, 'ENERGY TOO LOW FOR K-CAPTURE')               
              IF (PL1EX .GT. 0.) GOTO 330                               
              WRITE(36, 6030)                                           
 6030         FORMAT(20X, 'ENERGY TOO LOW FOR L1 CAPTURE')              
              IF (PL2 .GT. 0.) GOTO 340                                 
              WRITE(36, 6040)                                           
 6040         FORMAT(20X, 'ENERGY TOO LOW FOR L2 CAPTURE')              
              GOTO 99                                                   
C                                                                       
  320     CONTINUE                                                      
C         EKTOE = PKEX / PCAP                                           
C         ELTOEK = ELTOT / PKEX                                         
C         WRITE(36, 6050) EKTOE, ELTOEK                                 
C6050     FORMAT(20X, 'K-CAPTURE TO TOTAL CAPTURE RATIO = ', F7.5,      
C    1        10X, ' L/K CAPTURE RATIO', F10.4)                         
C                                                                       
  330     CONTINUE                                                      
C         R2TL1 = PL2 / PL1EX                                           
C         WRITE(36, 6060) R2TL1                                         
C6060     FORMAT(20X, 'L2/L1 CAPTURE RATIO = ', E10.3)                  
C                                                                       
  340     CONTINUE                                                      
C         EMNOTL = PMN / ELTOT                                          
C         WRITE(36, 6070) EMNOTL                                        
C6070     FORMAT(20X, 'MNO-CAPTURE RATIO TO L-CAPTURE RATIO = ', E10.3) 
          GOTO 99                                                       
C                                                                       
C-FIRST FORBIDDEN UNIQUE CAPTURE.                                       
C                                                                       
  360 PKU = PKEX * WEKSQ                                                
      PL1U = PL1EX * WEL1SQ                                             
      PL2U = PL2 * WEL2SQ                                               
C                                                                       
C-L3 TERM.                                                              
C                                                                       
      WEL3 = WE + WL3                                                   
      WEL3SQ = WEL3 * WEL3                                              
      PL3U = 0.                                                         
      IF (WEL3 .GT. 0.) PL3U = WEL3SQ * GL3R9                           
C                                                                       
C-MNO CAPTURE.                                                          
C                                                                       
      PMNU = PMN * WEMSQ                                                
      PM3 = 0.                                                          
      PN3 = 0.                                                          
      IF (IZ .LT. 18) GOTO 394                                          
          WEM3 = WE + WM3L(IZ)                                          
          WEM3SQ = WEM3 * WEM3                                          
          Z = IZ                                                        
          RM3 = 4.3E-2 + Z * (4.198E-3 - Z * 1.6E-5)                    
          PM3 = WEM3SQ * GL3R9 * RM3                                    
          IF (IZ .LT. 36) GOTO 394                                      
              RN3 = -5.27E-3 + Z * (2.352E-3 - Z * 9.85E-6)             
              WEN3 = WE + 1.                                            
              WEN3SQ = WEN3 * WEN3                                      
              PN3 = WEN3SQ * GL3R9 * RN3                                
  394 IF (ITYPE .EQ. 1) GOTO 395                                        
C                                                                       
C-SECOND FORBIDDEN UNIQUE.                                              
C                                                                       
          PKU = PKU * WEKSQ                                             
          PL1U = PL1U * WEL1SQ                                          
          PL2U = PL2U * WEL2SQ                                          
          PMNU = PMNU * WEMSQ                                           
          PL3U = PL3U * WEL3SQ * 3.33 333 333                           
          PM3 = PM3 * WEM3SQ * 3.33 333 333                             
          PN3 = PN3 * WEN3SQ * 3.33 333 333                             
C                                                                       
C-SUMMARIZE UNIQUE CAPTURE.                                             
C                                                                       
  395 ELTOTU = PL1U + PL2U + PL3U                                       
      PMNU = PMNU + PM3 + PN3                                           
      PCAPU = PKU + ELTOTU + PMNU                                       
      CAPTUR = 1.5708 * PCAPU                                           
      FCK = 1.5708 * PKU                                                
      FCL = 1.5708 * ELTOTU                                             
      FCMNO = 1.5708 * PMNU                                             
C                                                                       
C-PRINTING (OPTIONAL).                                                  
C                                                                       
      IF (IPRINT .LE. 0) GOTO 99                                        
      IF (PKEX .GT. 0.) GOTO 430                                        
          IF (PL3U .GT. 0.) GOTO 440                                    
              WRITE(36, 6080)                                           
 6080         FORMAT(20X, 'ENERGY TOO LOW FOR L3 CAPTURE ')             
              GOTO 460                                                  
C                                                                       
  430 CONTINUE                                                          
C     EKTOEU = PKU / PCAPU                                              
C     ELTKU = ELTOTU / PKU                                              
C     WRITE(36, 6050) EKTOEU, ELTKU                                     
  440 IF (PL1U .LE. 0.) GOTO 460                                        
C     R2TL1U = PL2U / PL1U                                              
C         WRITE(36, 6060) R2TL1U                                        
C         R3TL1U = PL3U / PL1U                                          
C         WRITE(36, 6090) R3TL1U                                        
C6090     FORMAT(20X, 'L3/L1 CAPTURE RATIO = ', E10.3)                  
  460 CONTINUE                                                          
C     EMNTLU = PMNU / ELTOTU                                            
C     WRITE(36, 6070) EMNTLU                                            
      GOTO 99                                                           
C                                                                       
  500 WRITE(36, 7000)                                                   
 7000 FORMAT(' ENERGY TOO LOW FOR THIS PROGRAM')                        
      CAPTUR = 0.                                                       
      FCK = 0.                                                          
      FCL = 0.                                                          
      FCMNO = 0.                                                        
C                                                                       
   99 RETURN                                                            
      END                                                               
      BLOCK DATA                                                        
C                                                                       
      REAL GKL(130)                                                     
      REAL GLUL(130)                                                    
      REAL FLUL(130)                                                    
      REAL GL3L(130)                                                    
      REAL RL(130)                                                      
C                                                                       
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         
C                                                                       
      DATA GKL, GLUL, FLUL, GL3L, RL/650 * 0.0/                         
C                                                                       
      END                                                               
      SUBROUTINE READWV                                                 
C                                                                       
C  READS WAVE FUNCTION DATA FOR USE BY WAVEFN.                          
C                                                                       
      REAL GKL(130), GK                                                 
      REAL GLUL(130), GL1                                               
      REAL FLUL(130), FL2                                               
      REAL GL3L(130), GL3                                               
      REAL RL(130), RMNO                                                
C                                                                       
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         
C                                                                       
      INTEGER IZ                                                        
C                                                                       
C     Radial wave functions for Z<5 added to LOGFT.DAT as per JKT's     
C       phone talks with MJM 890413                                     
C       It is not clear whether GK's include or GL1/GK=0.031 for Z=4 inc
C       exchange corrections                                            
      Save
C                                                                       
    1 READ(20, 100) IZ, GK, GL1, FL2, GL3, RMNO                         
  100 FORMAT(I3, 7X, 5F10.0)                                            
C                                                                       
      IF (IZ .LE. 0) RETURN                                             
          GKL(IZ) = GK                                                  
          GLUL(IZ) = GL1                                                
          FLUL(IZ) = FL2                                                
          GL3L(IZ) = GL3                                                
          RL(IZ) = RMNO                                                 
          GOTO 1                                                        
C                                                                       
      END                                                               
      SUBROUTINE WAVEFN(IZ, GK, GLU, FLU, GL3, RMNO)                    
C                                                                       
C  RETURNS WAVE FUNCTION DATA VALUES.                                   
C                                                                       
      REAL GKL(130), GK                                                 
      REAL GLUL(130), GLU                                               
      REAL FLUL(130), FLU                                               
      REAL GL3L(130), GL3                                               
      REAL RL(130), RMNO                                                
      REAL DG, DL, DF, DGL, DR                                          
C                                                                       
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         
C                                                                       
      INTEGER I, IZ, IM1, IP1, IN, INM1                                 
      Save
C                                                                       
C-INITIALIZATION.                                                       
C                                                                       
   10 GK = GKL(IZ)                                                      
      GLU = GLUL(IZ)                                                    
      FLU = FLUL(IZ)                                                    
      GL3 = GL3L(IZ)                                                    
      RMNO = RL(IZ)                                                     
C                                                                       
C-EXTRAPOLATION FOR MISSING DATA.                                       
C                                                                       
      IF (GK .NE. 0.) RETURN                                            
C                                                                       
C-FIND NEAREST LIBRARY VALUES.                                          
C                                                                       
      I = IZ                                                            
   20 I = I - 1                                                         
      IF (GKL(I) .EQ. 0.) GOTO 20                                       
      IM1 = I - 1                                                       
      DG = GKL(I) - GKL(IM1)                                            
      DL = GLUL(I) - GLUL(I - 1)                                        
      DF = FLUL(I) - FLUL(IM1)                                          
      DGL = GL3L(I) - GL3L(IM1)                                         
      DR = RL(I) - RL(IM1)                                              
      IP1 = I + 1                                                       
      DO 30 IN = IP1, IZ                                                
          INM1 = IN - 1                                                 
          GKL(IN) = GKL(INM1) + DG                                      
          GLUL(IN) = GLUL(INM1) + DL                                    
          FLUL(IN) = FLUL(INM1) + DF                                    
          GL3L(IN) = GL3L(INM1) + DGL                                   
          RL(IN) = RL(INM1) + DR                                        
   30     CONTINUE                                                      
      GOTO 10                                                           
      END                                                               
      INTEGER FUNCTION IPREC(STR)                                       
C                                                                       
C     Corrected subroutine to account for floating point notation       
C       (TWB. 921216)                                                   
C     Improved algorithms for calculating precision (TWB. 930212)       
C                                                                       
      CHARACTER*(*) STR                                                 
C                                                                       
      INTEGER I,IEND,IPER                                               
      CHARACTER*10  LOCAL                                               
C                                                                       
      INTEGER INDEXF,LENSTR                                             
      EXTERNAL INDEXF,LENSTR                                            
      INTEGER INDEX                                                     
      INTRINSIC INDEX                                                   
C                                                                       
      LOCAL=STR                                                         
      CALL SQZSTR(LOCAL, ' ')                                           
      IPER = INDEX(LOCAL, '.')                                          
      IF (IPER .GT. 0)THEN                                              
         IEND=INDEXF(LOCAL,IPER,'E')-1                                  
         IF(IEND .LE. 0)IEND=LENSTR(LOCAL)                              
         IPREC = IEND - IPER                                            
         IF(IPER .GT. 2)THEN                                            
            IPREC=IPREC+IPER-1                                          
         ELSE                                                           
            IF(IPER .EQ. 2)THEN                                         
               IF(LOCAL(1:1) .NE. '0')THEN                              
                  IPREC=IPREC+1                                         
               ELSE                                                     
                  DO 100 I=IPER+1,IEND                                  
                     IF(LOCAL(I:I) .NE. '0')GOTO 200                    
                     IPREC=IPREC-1                                      
100               CONTINUE                                              
200               CONTINUE                                              
               ENDIF                                                    
            ENDIF                                                       
         ENDIF                                                          
      ELSE                                                              
         IEND=INDEX(LOCAL,'E')                                          
         IF(IEND .LE. 0)THEN                                            
            IPREC=LENSTR(LOCAL)                                         
         ELSE                                                           
            IPREC=IEND-1                                                
         ENDIF                                                          
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE UNCALP(DX,UNCERT,IFIELD)                               
C                                                                       
C   If UNCERT is the given non-numeric uncertainty then decide          
C   what the correct non-numeric uncertainty for the field IFIELD       
C   is.  And Returns the uncertainty in DX.                             
C     IFIELD=1   OUTPUTFIELD LOGFT                                      
C            4               IB                                         
C            5               IE                                         
C                                                                       
      INTEGER IFIELD                                                    
      CHARACTER*2 UNCERT                                                
      CHARACTER*2 DX                                                    
C                                                                       
      IF(UNCERT.EQ.'AP' .OR. UNCERT.EQ.'CA' .OR. UNCERT.EQ.'SY') THEN   
         DX=UNCERT                                                      
         RETURN                                                         
      ENDIF                                                             
C                                                                       
C   IFIELD=1   LOGFT                                                    
C                                                                       
      IF(IFIELD.EQ.1) THEN                                              
         IF(UNCERT(1:1).EQ.'L') THEN                                    
            DX='G'//UNCERT(2:2)                                         
         ELSE                                                           
            DX='L'//UNCERT(2:2)                                         
         ENDIF                                                          
         RETURN                                                         
      ENDIF                                                             
C                                                                       
C   IFIELD=4 OR 5   IB OR IE                                            
C                                                                       
      IF(IFIELD.EQ.4 .OR. IFIELD.EQ.5) THEN                             
         DX=UNCERT                                                      
         RETURN                                                         
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      SUBROUTINE CENTER(STR,IFIELD)                                     
C                                                                       
C   Take STR and center its characters in STR(1:IFIELD)                 
C                                                                       
      CHARACTER*(*) STR                                                 
      INTEGER       IFIELD                                              
C                                                                       
      INTEGER I,LAST                                                    
C                                                                       
      INTEGER LENSTR                                                    
      EXTERNAL LENSTR                                                   
C                                                                       
      CALL LBSUP(STR)                                                   
      LAST=LENSTR(STR)                                                  
      IF(LAST.GE.IFIELD) RETURN                                         
      I=IFIELD - (IFIELD-LAST)/2                                        
      CALL PADLFT(STR,I)                                                
      RETURN                                                            
      END                                                               
        REAL FUNCTION FNEW(X)                                           
C                                                                       
C  Short form of the Fermi function used for B+- and associated IB.     
C    This function attempts to obtain the Fermi function by polynomial  
C    interpolation.  If the interpolation is not successful, the value  
C    is obtained from FUNCTION F and stored in FSAV for later           
C    interpolations.  The parameter N should be reset to 1 with         
C    each call to SETUP.                                                
C                                                                       
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************
C*                           Present Code                               
C*                           ------------                               
C*  F       LOCATE  POLINT                                              
C*                                                                      
C*                        FORTRAN 77 Supplied                           
C*                        -------------------                           
C*  ABS                                                                 
C*                                                                      
C***********************************************************************
        REAL X                                                          
C                                                                       
        INTEGER NSAV,NSET,NSTEPM,NTEST                                  
C        PARAMETER (NSAV=200,NSET=3,NSTEPM=6)                           
        PARAMETER (NSAV=200,NSET=3,NSTEPM=6,NTEST=NSAV/10)              
C                                                                       
        REAL F                                                          
        EXTERNAL F                                                      
      REAL ABS,ALOG10                                                   
      DOUBLE PRECISION DABS,DBLE                                        
      INTRINSIC ABS,ALOG10,DABS,DBLE                                    
C                                                                       
        REAL DUM,EPREC                                                  
        COMMON/PREC1/DUM,EPREC                                          
C                                                                       
        INTEGER N                                                       
        COMMON /FCOMM/N                                                 
C                                                                       
        INTEGER I,J,JJ,LOWER,NSTEP,THPCK                                
        REAL DY,TEMP1,TEMP2,Y,ESAV(NSAV),FSAV(NSAV)                     
C                                                                       
      LOGICAL ISLOW,ISHIG                                               
      REAL LOWZER,HGHZER                                                
      Save
C                                                                       
      IF(N .EQ. 1)THEN                                                  
         ISLOW=.FALSE.                                                  
         ISHIG=.FALSE.                                                  
      ELSE                                                              
         IF(ISLOW)THEN                                                  
            IF(X .LE. LOWZER)THEN                                       
               FNEW=0.                                                  
               RETURN                                                   
            ENDIF                                                       
         ENDIF                                                          
         IF(ISHIG)THEN                                                  
            IF(X .GE. HGHZER)THEN                                       
               FNEW=0.                                                  
               RETURN                                                   
            ENDIF                                                       
         ENDIF                                                          
      ENDIF                                                             
        IF(N .GE. NSET .AND. X .LE. ESAV(N-1))THEN                      
           CALL LOCATE(ESAV,N-1,X,LOWER)                                
C          Changed comparison to double precision (TWB. 930112)         
           IF(LOWER .EQ. 0 .AND. ABS(ALOG10(X)-ALOG10(ESAV(1))) .LE. 1.)
     2       THEN                                                       
              IF(DABS(DBLE(X)/DBLE(ESAV(1))-1.D+0) .LT. DBLE(EPREC))THEN
                 FNEW=FSAV(1)                                           
                 RETURN                                                 
              ENDIF                                                     
           ENDIF                                                        
C          Rewrote comparisons in double precision and tightened up     
C            the tests - picking wrong value sometimes (TWB. 930112)    
           IF(LOWER .NE. 0)THEN                                         
              IF(DABS(DBLE(X)-DBLE(ESAV(LOWER))) .LE.                   
     2          DABS(DBLE(X)-DBLE(ESAV(LOWER+1))))THEN                  
                 THPCK=LOWER                                            
              ELSE                                                      
                 THPCK=LOWER+1                                          
              ENDIF                                                     
              IF(DABS(DBLE(X)/DBLE(ESAV(THPCK))-1.D+0)                  
     2          .LT. DBLE(EPREC))THEN                                   
                 FNEW=FSAV(THPCK)                                       
                 RETURN                                                 
              ENDIF                                                     
              NSTEP=MIN0(N-1,NSTEPM)                                    
              IF(LOWER .GT. N-1-NSTEP)THEN                              
                 LOWER=N-1-NSTEP                                        
              ELSE                                                      
                 LOWER=LOWER-NSTEP/2                                    
              ENDIF                                                     
              IF(LOWER .GE. 1)THEN                                      
                 NSTEP=MIN0(N-1-LOWER,NSTEPM)                           
                 CALL POLINT(ESAV(LOWER),FSAV(LOWER),NSTEP,X,Y,DY)      
C                Changed check on precision to double precision -       
C                  convergence problems (TWB. 930212)                   
                 IF(Y .NE. 0.)THEN                                      
                    IF(DABS(DBLE(DY)/DBLE(Y)) .LE. EPREC)THEN           
                       FNEW=Y                                           
                       RETURN                                           
                    ENDIF                                               
                 ENDIF                                                  
              ENDIF                                                     
           ENDIF                                                        
        ENDIF                                                           
        FNEW=F(X)                                                       
      IF(N .GT. NTEST)THEN                                              
         IF(FSAV(2) .EQ. 0.)THEN                                        
            DO 300 I=(N-1)/2,2,-1                                       
               IF(FSAV(I) .EQ. 0. .AND. I .GT. 1)THEN                   
                  ISLOW=.TRUE.                                          
                  LOWZER=ESAV(I)                                        
                  GOTO 310                                              
               ENDIF                                                    
300         CONTINUE                                                    
310         CONTINUE                                                    
            JJ=1                                                        
            DO 320 J=I,N-1                                              
               ESAV(JJ)=ESAV(J)                                         
               FSAV(JJ)=FSAV(J)                                         
               JJ=JJ+1                                                  
320         CONTINUE                                                    
            N=JJ                                                        
         ENDIF                                                          
         IF(FSAV(N-2) .EQ. 0.)THEN                                      
            DO 400 I=(N-1)/2,N-2                                        
               IF(FSAV(I) .EQ. 0. .AND. I .LT. N-1)THEN                 
                  ISHIG=.TRUE.                                          
                  HGHZER=ESAV(I)                                        
                  GOTO 410                                              
               ENDIF                                                    
400         CONTINUE                                                    
410         CONTINUE                                                    
            N=I+1                                                       
         ENDIF                                                          
      ENDIF                                                             
      IF(N .GT. NSAV)RETURN                                             
        ESAV(N)=X                                                       
        FSAV(N)=FNEW                                                    
        IF(N .GT. 1)THEN                                                
           DO 200 I=1,N-1                                               
              DO 100 J=I+1,N                                            
                 IF(ESAV(I) .GT. ESAV(J))THEN                           
                    TEMP1=ESAV(J)                                       
                    TEMP2=FSAV(J)                                       
                    ESAV(J)=ESAV(I)                                     
                    FSAV(J)=FSAV(I)                                     
                    ESAV(I)=TEMP1                                       
                    FSAV(I)=TEMP2                                       
                 ENDIF                                                  
100           CONTINUE                                                  
200        CONTINUE                                                     
        ENDIF                                                           
        N=N+1                                                           
        RETURN                                                          
        END                                                             
        SUBROUTINE LOCATE(XX,N,X,J)                                     
C                                                                       
C  Given an array XX of length N, and a given value X, returns a value J
C    such that X is between XX(J) and XX(J+1).  XX must be monotonic,   
C    either increasing or decreasing.  J=0 or J=N is returned to indicat
C    that X is out of the range.                                        
C    [Numerical Recipes (The Art of Scientific Computing.  W.H. Press,  
C    et al.  Cambridge University Press (NY, 1986), p.90]               
C                                                                       
        INTEGER N,J                                                     
        REAL XX(N),X                                                    
C                                                                       
        INTEGER JL,JU,JM                                                
C                                                                       
        JL=0                                                            
        JU=N+1                                                          
10      IF(JU-JL .GT. 1)THEN                                            
           JM=(JU+JL)/2                                                 
           IF((XX(N) .GT. XX(1)) .EQV. (X .GT. XX(JM)))THEN             
              JL=JM                                                     
           ELSE                                                         
              JU=JM                                                     
           ENDIF                                                        
           GOTO 10                                                      
        ENDIF                                                           
        J=JL                                                            
        RETURN                                                          
        END                                                             
      SUBROUTINE ZBLANK(X,DX)                                           
C                                                                       
C   if x is zero then x and dx are set to blank                         
C                                                                       
      CHARACTER *(*) X,DX                                               
C                                                                       
      INTEGER I,J                                                       
C                                                                       
      INTEGER LENSTR                                                    
      EXTERNAL LENSTR                                                   
C                                                                       
      J=LENSTR(X)                                                       
      DO 100 I=1,J                                                      
         IF(X(I:I).NE.'.' .AND. X(I:I).NE.' ' .AND.                     
     1      X(I:I).NE.'0') RETURN                                       
  100 CONTINUE                                                          
      X=' '                                                             
      DX=' '                                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE PROCE                                                  
C                                                                       
C   process E card                                                      
C                                                                       
      COMMON /CARDIM/CARD,CARD2,STARS                                   
      CHARACTER*80  CARD                                                
      CHARACTER*80  CARD2                                               
      CHARACTER* 8  STARS                                               
C                                                                       
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          
     1    IBONLY,ZERUNC,EK,NUMP                                         
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             
      REAL    DPERC, DPINT,DEINT                                        
      REAL    PERC, PINT, EINT                                          
      REAL    EK(5)                                                     
C                                                                       
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              
      CHARACTER* 2  TYPE                                                
      CHARACTER     ATI*10,  DATI*2                                     
      CHARACTER*2   UNCERT                                              
C                                                                       
      INTEGER J2PREC                                                    
      CHARACTER*10  STA                                                 
      CHARACTER* 2  STB                                                 
C                                                                       
      INTEGER IPREC                                                     
      EXTERNAL IPREC                                                    
      Save
C                                                                       
 9006 FORMAT(11X,A, /,'0')                                              
 9007 FORMAT(35X,A )                                                    
C                                                                       
         WRITE(36, FMT='(A)')                                           
         IZ = IABS(IZ)                                                  
         TYPE=CARD(78:79)                                               
C                                                                       
C     TI field (65-74)                                                  
C                                                                       
         STA=CARD(65:74)                                                
         IF(STA .NE. ' ') THEN                                          
C     TI VALUE GIVEN - Use TI for total intensity                       
             STB=CARD(75:76)                                            
C                                                                       
C-SAVE TOTAL INTENSITY FROM THE TI-FIELD.                               
C                                                                       
             ATI=STA                                                    
             DATI=STB                                                   
             UNCERT=STB                                                 
C                                                                       
C     SAVE PRECISION OF INPUT                                           
             JPREC = IPREC(STA)                                         
             CALL CNVS2U(STA, STB, PERC, DPERC)                         
C     NOTE IF DPERC = 0.0 (MISSING UNCERT)                              
C        Program was wrongly setting ZERUNC when pure 100% decay        
C          (TWB. 930212)                                                
         IF ((DPERC .EQ. 0.0 .AND. PERC .NE. 100.) .OR. STB .NE. ' ')   
     2     ZERUNC = 1                                                   
             EINT = PERC                                                
             DEINT = DPERC                                              
C                                                                       
C-REPLACE OLD IB, IE WITH BLANKS.                                       
C                                                                       
             CARD(22:41)=' '                                            
         ELSE                                                           
C                                                                       
C-NO ENTRY GIVEN IN THE TI-FIELD.                                       
C-DECODE POSITRON INTENSITY.(IB) (22-29)                                
C                                                                       
             STA=CARD(22:29)                                            
C     SAVE PRECISION OF INPUT                                           
             JPREC = IPREC(STA)                                         
             STB=CARD(30:31)                                            
             CALL CNVS2U(STA, STB, PINT, DPINT)                         
             UNCERT=STB                                                 
C                                                                       
C-DECODE CAPTURE INTENSITY.(IE) (32-39)                                 
C   (if both TI and IB are blank, then use IE's uncertainty for UNCERT  
C                                                                       
             STA=CARD(32:39)                                            
C     SAVE PRECISION OF INPUT                                           
             J2PREC = IPREC(STA)                                        
C     USE GREATER PRECISION                                             
             IF (J2PREC .GT. JPREC) JPREC = J2PREC                      
             STB=CARD(40:41)                                            
             CALL CNVS2U(STA, STB, EINT, DEINT)                         
C                                                                       
C   If IB uncertainty is numeric or IB's are not given then             
C   save STB for UNCERTainty                                            
C                                                                       
             IF(PINT.EQ.0. .AND. UNCERT.EQ.' ') UNCERT=STB              
C           Uncertainty from IE field should only be used if dominate   
C             (TWB. 930212)                                             
             IF(DEINT.GT.0 .AND. EINT .GT. PINT) UNCERT=STB             
C                                                                       
C-FIND TOTAL INTENSITY.                                                 
C                                                                       
             PERC = PINT + EINT                                         
             DPERC = SQRT(DPINT * DPINT + DEINT * DEINT)                
C                                                                       
C   if both IB(PINT) and IE(EINT) are given, blank out IB, IE fields    
C                                                                       
             IF (PINT .NE. 0.0 .AND. EINT .NE. 0.0) THEN                
                 CALL CNVU2S(PERC, DPERC, ATI, 10, DATI, 2)             
                 CARD(22:41)=' '                                        
             ENDIF                                                      
C     NOTE IF DPINT OR DEINT = 0.0                                      
C     BUT THEIR CORRESP VALUES NOT = 0.0 and not equal to 100           
             ZERUNC = 0                                                 
C           Program was not correctly setting all ZERUNC=1 cases        
C             and not calculating uncertainty when 100% decay           
C             (TWB. 930112)                                             
             IF ((DPINT .EQ. 0.0 .AND. PINT .NE. 0.0                    
     2         .AND. PINT .NE. 100.)                                    
     3         .OR. (uncert(1:1) .GE. 'A' .AND. uncert(1:1) .LE. 'Z'))  
     4         ZERUNC = 1                                               
             IF ((DEINT .EQ. 0.0 .AND. EINT .NE. 0.0                    
     2         .AND. EINT .NE. 100.)                                    
     3         .OR. (uncert(1:1) .GE. 'A' .AND. uncert(1:1) .LE. 'Z'))  
     4         ZERUNC = 1                                               
             IF (EINT .EQ. 0.) THEN                                     
                 WRITE(36, 9006)'NO CAPTURE INTENSITY GIVEN.'           
                 IF(PINT.NE.0.) IBONLY = 1                              
             ENDIF                                                      
             IF (PINT .NE. 0.) THEN                                     
C                                                                       
C   IB (PINT) given                                                     
C                                                                       
                 IF (EINT.NE.0.) THEN                                   
                    IF ((DEINT .NE. 0.) .AND. (DPINT .NE. 0.)) THEN     
                       DPERC = PERC * AMIN1(DEINT / EINT, DPINT / PINT) 
                    ENDIF                                               
                 ENDIF                                                  
                 RETURN                                                 
             ELSE                                                       
C                                                                       
C-IF NO POSITRON INTENSITY, THEN ASSUME IE IS TOTAL.                    
C-SAVE TOTAL INTENSITY IN THE TI-FIELD.                                 
C                                                                       
                ATI=STA                                                 
                DATI=STB                                                
                WRITE(36,9007)                                          
     1              'IE is assumed to be the total intensity'           
             ENDIF                                                      
         ENDIF                                                          
C  use last EK if there are more                                        
C        Program was outputing extraneous messages (TWB. 930212)        
         IF (EK(NUMP) .GT. 1022. .AND. pint .EQ. 0.) WRITE(36, 9007)    
     1      'NO POSITRON INTENSITY GIVEN'                               
      RETURN                                                            
      END                                                               
      SUBROUTINE WECARD                                                 
C                                                                       
C   Place IE, IB in proper fields on the new first E-Card               
C                                                                       
      COMMON /CARDIM/CARD,CARD2,STARS                                   
      CHARACTER*80  CARD                                                
      CHARACTER*80  CARD2                                               
      CHARACTER* 8  STARS                                               
C                                                                       
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          
     1    IBONLY,ZERUNC,EK,NUMP                                         
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             
      REAL    DPERC, DPINT,DEINT                                        
      REAL    PERC, PINT, EINT                                          
      REAL    EK(5)                                                     
C                                                                       
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              
      CHARACTER* 2  TYPE                                                
      CHARACTER     ATI*10,  DATI*2                                     
      CHARACTER*2   UNCERT                                              
C                                                                       
      COMMON /WENUM/ AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM,     
     1    FCLP,FCMNM, FCMNO, FCMNP,NPOS,ETOP                            
      INTEGER NPOS                                                      
      REAL   AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM, FCLP,       
     1    FCMNM, FCMNO, FCMNP,ETOP                                      
C                                                                       
      INTEGER LPREC,DELTMP,theprc                                       
      REAL EKTOT,DEKTOT,ELTOT,DELTOT,EMTOT,DEMTOT                       
      REAL EKTOTM,EKTOTP,ELTOTM,ELTOTP,EMTOTM,EMTOTP                    
      REAL DPREC                                                        
      CHARACTER*2  STB                                                  
      Character seint*8,dseint*2                                        
      Character spint*8,dspint*2                                        
      Character*11 scard2                                               
C                                                                       
      INTEGER INT,LEN,NINT                                              
      REAL ALOG10                                                       
      INTRINSIC ALOG10,INT,LEN,NINT                                     
C                                                                       
      INTEGER LENSTR                                                    
      EXTERNAL LENSTR                                                   
      Save
C                                                                       
         EKTOT = FCK / AREA                                             
         EKTOTP = FCKP / AREAP                                          
         EKTOTM = EKTOT                                                 
         IF (AREAM .GT. 0.) EKTOTM = FCKM / AREAM                       
         DEKTOT = AMAX1(ABS(EKTOTP - EKTOT), ABS(EKTOT - EKTOTM))       
         ELTOT = FCL / AREA                                             
         ELTOTP = FCLP / AREAP                                          
         ELTOTM = ELTOT                                                 
         IF (AREAM .GT. 0.) ELTOTM = FCLM / AREAM                       
         DELTOT = AMAX1(ABS(ELTOT - ELTOTP), ABS(ELTOT - ELTOTM))       
         EMTOT = FCMNO / AREA                                           
         EMTOTP = FCMNP / AREAP                                         
         EMTOTM = EMTOT                                                 
         IF (AREAM .GT. 0.) EMTOTM = FCMNM / AREAM                      
         DEMTOT = AMAX1(ABS(EMTOT - EMTOTP), ABS(EMTOT - EMTOTM))       
         WRITE(36, 9019) EKTOT, DEKTOT, ELTOT, DELTOT, EMTOT, DEMTOT    
 9019 FORMAT(20X, 'K/(EC+B+)=', 1PE10.4, '+-', E9.2,                    
     1    5X, 'L/(EC+B+)=', 1PE10.4, '+-', E9.2,                        
     2    5X, 'MNO/(EC+B+)=', 1PE10.4, '+-', E9.2)                      
C                                                                       
C     NPOS=0    EC only decay                                           
C                                                                       
         Call Lbsup(ati)                                                
         IF (NPOS .EQ. 0) THEN                                          
            If(Lenstr(ati) .GT. LEN(seint))Write(36,fmt='(3A)')         
     2        ' Warning: ',ati(1:Lenstr(ati)),' will be truncated'      
            seint=ATI                                                   
            dseint=DATI                                                 
            spint=STARS                                                 
            GO TO 510                                                   
         ENDIF                                                          
C        Reduced limit to output IB and IE from 0.01% to 0.001%         
C          (TWB. 930212)                                                
         IF ((EINT .LT. 0.001) .AND. (ETOP .LT. 0.001)) THEN            
            If(Lenstr(ati) .GT. LEN(spint))Write(36,fmt='(3A)')         
     2        ' Warning: ',ati(1:Lenstr(ati)),' will be truncated'      
             spint=ATI                                                  
             dspint=DATI                                                
             seint=STARS                                                
             GOTO 510                                                   
         ENDIF                                                          
         IF ((PINT .LT. 0.001) .AND. (ETOP .GT. 1000.)) THEN            
            seint=ATI                                                   
            dseint=DATI                                                 
            spint=STARS                                                 
            GO TO 510                                                   
         ENDIF                                                          
C                 TO PREVENT WRITING VALUES WHOSE UNCERTAINTY IS        
C                 LARGER THAN THE VALUE BY A FACTOR OF 5                
         IF (DEINT / EINT .GT. 5.0) ZERUNC = 1                          
         IF (DPINT / PINT .GT. 5.0) ZERUNC = 1                          
         IF (ZERUNC .EQ. 0) THEN                                        
            CALL CNVU2S(EINT, DEINT, seint, 8, dseint, 2)               
            CALL CNVU2S(PINT, DPINT, spint, 8, dspint, 2)               
         ELSE                                                           
C                 PRINT USING CALCULATED PRECISION                      
C           Logic was mixed up. Wrong value of JPREC was being used and 
C             wrong uncertainty was being output for the smaller of     
C             EINT and PINT (TWB. 921216)                               
C           Continued refinement of logic (TWB. 930212)                 
            If(eint .GT. 1.)Then                                        
               lprec=INT(ALOG10(EINT))+1                                
            Else                                                        
               LPREC=INT(ALOG10(EINT))                                  
            Endif                                                       
            theprc=lprec-jprec                                          
            DPREC=3.0*10.0**theprc                                      
            DELTMP=NINT(DEINT/10.0**theprc)                             
1000        CONTINUE                                                    
            IF(DELTMP .GT. 25)THEN                                      
               theprc=theprc+1                                          
               DPREC=DPREC*10.                                          
               DELTMP=NINT(DEINT/10.0**theprc)                          
               GoTo 1000                                                
            ENDIF                                                       
            CALL CNVU2S(EINT, DPREC, seint, 8, dseint, 2)               
            CALL KNVI2S(DELTMP,dseint,0)                                
            If(pint .GT. 1.)Then                                        
               lprec=INT(ALOG10(PINT))+1                                
            Else                                                        
               LPREC=INT(ALOG10(PINT))                                  
            Endif                                                       
            theprc=lprec-jprec                                          
            DPREC=3.0*10.0**theprc                                      
            DELTMP=NINT(DPINT/10.0**theprc)                             
1100        CONTINUE                                                    
            IF(DELTMP .GT. 25)THEN                                      
               theprc=theprc+1                                          
               DPREC=DPREC*10.                                          
               DELTMP=NINT(DPINT/10.0**theprc)                          
               GoTo 1100                                                
            ENDIF                                                       
            CALL CNVU2S(PINT, DPREC, spint, 8, dspint, 2)               
            CALL KNVI2S(DELTMP,dspint,0)                                
            IF(LENSTR(UNCERT) .EQ. 0)THEN                               
               dseint = ' '                                             
               dspint = ' '                                             
            ELSE                                                        
               IF(UNCERT(1:1) .GE. 'A' .AND. UNCERT(1:1) .LE. 'Z')THEN  
                  dseint=UNCERT                                         
                  dspint=UNCERT                                         
               ENDIF                                                    
            ENDIF                                                       
         ENDIF                                                          
  510    CONTINUE                                                       
C                                                                       
C   if the number is all 0, then blank out                              
C                                                                       
         CALL ZBLANK(spint,dspint)                                      
         CALL ZBLANK(seint,dseint)                                      
         IF (seint(1:8) .NE. STARS) THEN                                
             CALL LBSUP(seint)                                          
             CARD(32:39)=seint                                          
             CARD(40:41)=dseint                                         
         ENDIF                                                          
         IF (spint(1:8) .NE. STARS) THEN                                
             CALL LBSUP(spint)                                          
             CARD(22:29)=spint                                          
             CARD(30:31)=dspint                                         
         ENDIF                                                          
         CALL CENTER(CARD(22:29),8)                                     
         CALL CENTER(CARD(32:39),8)                                     
C                                                                       
C-PREPARE NEW SECOND E-CARD.                                            
C                                                                       
C     Do not output capture fractions if IE is not output (TWB 930212)  
      If(card(32:39) .NE. ' ')Then                                      
         If(ektot .GT. 0.)Call Addfrac(card2,ektot,dektot,'CK')         
         If(eltot .GT. 0.)Call Addfrac(card2,eltot,deltot,'CL')         
         If(emtot .GT. 0.)Call Addfrac(card2,emtot,demtot,'CM+')        
      Endif                                                             
      RETURN                                                            
      END                                                               
      Subroutine Addfrac(card,frac,dfrac,text)                          
C     Adds electron-capture fractions to the new "S E" record           
C        card   "S E" record                                            
C        frac   capture fraction                                        
C        dfrac  uncertainty in capture fraction                         
C        text   type of fraction                                        
C                                                                       
      Character*(*)card                                                 
      Real frac,dfrac                                                   
      Character*(*) text                                                
C                                                                       
      Integer lprec,start                                               
      Character*2  dsx                                                  
      Character*11 sx                                                   
C                                                                       
      Integer Lenstr                                                    
      External Lenstr                                                   
C                                                                       
      Integer INDEX,NINT                                                
      Real ALOG10                                                       
      Intrinsic ALOG10,INDEX,NINT                                       
C                                                                       
      If(Lenstr(card) .LT. 10)Then                                      
         start=10                                                       
      Else                                                              
         start=Lenstr(card)+1                                           
      EndIf                                                             
C                                                                       
      If(dfrac/frac .LE. 0.0001)Then                                    
         dfrac=0.001*frac                                               
         Call Cnvu2s(frac,0.0,sx,11,dsx,0)                              
      ElseIf(dfrac/frac .LE. 0.001)Then                                 
         Call Cnvu2s(frac,dfrac,sx,11,dsx,-1)                           
      Else                                                              
         Call Cnvu2s(frac,dfrac,sx,11,dsx,2)                            
         Call Lbsup(sx)                                                 
         Call Lbsup(dsx)                                                
         Call Addstr(sx,Lenstr(sx)+2,dsx)                               
      EndIf                                                             
      If(INDEX(sx,'*') .GT. 0)Then                                      
         lprec=NINT(ALOG10(frac)-ALOG10(dfrac))                         
         Call Cnvu2s(frac,0.0,sx,11,dsx,-lprec)                         
      Endif                                                             
      If(INDEX(sx,'*') .EQ. 0)Then                                      
         card(start:)=text                                              
         start=start+Lenstr(text)                                       
         Call Addstr(card,start,'=')                                    
         Call Lbsup(sx)                                                 
         start=start+1                                                  
         Call Addstr(card,start,sx)                                     
         If(text .NE. 'CM+')Then                                        
            start=start+Lenstr(sx)                                      
            Call Addstr(card,start,'$')                                 
         EndIf                                                          
      EndIf                                                             
C                                                                       
      Return                                                            
      End                                                               

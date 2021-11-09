C***********************************************************************01 00010
C*                                                                      01 00020
C*    PROGRAM LOGFT                                                     01 00030
C*                                                                      01 00040
C*    VERSION 1 SUPPLIED BY ORNL.                                       01 00050
C*    VERSION 2A(43) AS OF  6-DEC-77. CONVERT TO MACHINE DEPENDENT CODE.01 00060
C*    VERSION 2A(44) AS OF  3-JAN-78. FIX IF ERROR.                     01 00070
C*    VERSION 3(45)  AS OF  4-JAN-78. REDUCE TO SINGLE PRECISION.       01 00080
C*    VERSION 3(50)  AS OF 31-JAN-78. REPLACE MIN1 WITH AMIN1.          01 00090
C*    VERSION 3(51)  AS OF  1-MAR-78. MATCH NEW (NOV. '77) NDP PGM.     01 00100
C*    VERSION 3(52)  AS OF 10-JUL-78. BRING UP TO NEW FORSTR LEVEL.     01 00110
C*    VERSION 3(53)  AS OF  2-AUG-78. FIX STMT 6160 TO BE F7.3 NOT F6.3.01 00120
C*    VERSION 3(54)  AS OF 12-DEC-79. HANDLE MISSING UNCERT. FOR INTENS.01 00130
C*    VERSION 3(55)  AS OF 12-DEC-79. CORRECT STMT 1274 FOR VAR. LEN.   01 00140
C*    VERSION 3(56)  AS OF 12-DEC-79. CHANGE K,L,M+ TO CK,CL,CM+.       01 00150
C*    VERSION 3(57)  AS OF 12-DEC-79. RETAIN PREC. OF IE, IB.           01 00160
C*    VERSION 4(60)  AS OF 11-FEB-81. ADD SUBR. CNVUXS AND CHANGE CALLS.01 00170
C*    VERSION 4(61)  AS OF 26-MAY-81. REMOVE CNVUXS. CNVU2S NOW DOES IT.01 00180
C*    VERSION 4(62)  AS OF 26-MAY-81. REPLACE CALL TO INSERT WITH PUTIN.01 00190
C*    VERSION 4(63)  AS OF 27-MAY-81. USE DIALOG=LINE FOR DEC.          01 00200
C*    VERSION 5(64)  AS OF 20-AUG-81. PLACE NUCID ON 2B AND 2E CARDS.   01 00210
C*    VERSION 5(65)  AS OF  2-SEP-81. REMOVE LAST $ ON CONT CARDS.      01 00220
C*    VERSION 5(66)  AS OF 23-MAY-84. FIX NEW E CARD GENERATION.        01 00230
C*    VERSION 6               JAN-86. CONVERT TO FORTRAN 77             01 00240
C*    VERSION 6(1)          6-AUG-86. Added VAX MDC                     01 00250
C*    VERSION 6(2)          2-DEC-86. Added IbmPC MDC. OVERFLOW IN DEINT01 00260
C*                                    FIXED.                            01 00270
C*    VERSION 6(3)          2-MAR-87. 2 B, 2 E to S B, S E for cont.    01 00280
C*                           records                                    01 00290
C*    VERSION 6(4)          2-NOV-87. VAX mdc READONLY in OPEN data file01 00300
C*    VERSION 7             6-MAR-89. Retain alpha uncertainties.  PN   01 00310
C*                           records. Mutiple Parent cards. RADLST's    01 00320
C*                           integration some output format mods. Clean 01 00330
C*                           up code etc.                               01 00340
C*    VERSION 7(1)  20-APR-89.  BKL,BL1L (Z<5) data values corrected by 01 00350
C*    VERSION 7(2)     AUG-89.  further fortran cleanup.                01 00360
C*    VERSION 7(3)  14-Dec-89.  Correct convergence problems in QROMB   01 00370
C*                              Fuction F modified.  (T.Burrows)        01 00380
C*    VERSION 7(4)   6-JUN-90.  Convert to IBM PC versin.  Some cleanup.01 00390
C*    VERSION 7(5)  14-SEP-90.  IBM version correction. IBONLY set      01 00400
C*                           condition corrected.                       01 00410
C*    VERSION 7(6)  15-OCT-90   Old integration package. Created PROCE  01 00420
C*                     and WECARD subroutines from sections of MAIN.    01 00430
C*                     Corrected TYPSTR declaration, Q(NUMP) bound error01 00440
C*    VERSION 7(7)  22-MAY-91   Correct value assignments to new card.  01 00450
C*    VERSION 7(8)  26-NOV-91   Correct SKIP1 setting when level E nonnu01 00460
C*    VERSION 7(9)  14-Oct-92.  Restored RADLST integration package     01 00470
C*                           (run-time stack overflow problem on IBM-PC 01 00480
C*                           solved with "/ST:4096" on link).           01 00490
C*                              Corrected statement out of order in     01 00500
C*                           function F(W) and use of consecutive       01 00510
C*                           arithmetic operators in subroutine POLINT  01 00520
C*                              Restored some variable declarations     01 00530
C*                           (commented out) to correspond to commented 01 00540
C*                           out sections of code                       01 00550
C*                              Added Machine coding for ANS            01 00560
C*                           (T.W. Burrows)                             01 00570
C*    VERSION 7(10) 16-Dec-92.  Tightened convergence tests from        01 00580
C*                            EPS=1.E-5, EPS1=10.*EPS to EPS=5.E-6,     01 00590
C*                            EPS1=EPS and rewrote check on convergence 01 00600
C*                            in QROMB after intercomparison with CML   01 00610
C*                            code BETA.                                01 00620
C*                              Finished typing all variables.          01 00630
C*                              Changed heurestic check on W0 from 1.2  01 00640
C*                            to 2.0                                    01 00650
C*                              Protected against floating overflows in 01 00660
C*                            calculating DETOP                         01 00670
C*                              Rewrote logic in calculating PINT and   01 00680
C*                            EINT to protect against unrealistic       01 00690
C*                            uncertainties when ETOP large or machine  01 00700
C*                            roundoff when ETOP small                  01 00710
C*                              Corrected integer function IPREC to     01 00720
C*                            account for floating point notation       01 00730
C*                              Corrected logic for outputing IB and    01 00740
C*                            IE when large uncertainties               01 00750
C*                            (T.W. Burrows)                            01 00760
C*    VERSION 7(11) 17-Feb-93.  Finished correcting convergence problems01 00770
C*                            by judicious addition of double precision 01 00780
C*                              Corrected:                              01 00790
C*                            a. problem when half-life units at end of 01 00800
C*                              field and neglect of "?" in T field     01 00810
C*                            b. neglect of PN record                   01 00820
C*                            c. neglect of parent and level energies of01 00830
C*                              the form "0+X"                          01 00840
C*                            d. overflow on branching ratio output     01 00850
C*                              Added:                                  01 00860
C*                            a. more terminal output to notify user of 01 00870
C*                              possible problems                       01 00880
C*                            b. check of NB*BR versus NBBR             01 00890
C*                            c. warning when NB=1/BR assumed           01 00900
C*                            d. Check for "E" or "B" continuation      01 00910
C*                              records other then "2" or "S"           01 00920
C*                            e. Improved algorithms for significant    01 00930
C*                              digits                                  01 00940
C*                            f. Minor cleanup                          01 00950
C*    VERSION 7(12) 10-May-93 a. Added checking on NUCID                01 00960
C*                            b. Corrected calculation of A for Z>105   01 00970
C*                            c. Corrected logic error which did not    01 00980
C*                               suppress new output when non-numeric   01 00990
C*                               uncertainty on E(level)                01 01000
C*                            d. Added check for unplaced or redundant  01 01010
C*                               E or B records                         01 01020
C*                            e. Added check for calculated transition  01 01030
C*                              energies less than or equal to 0        01 01040
C*                            f. Delinted using FLINT 2.83              01 01050
C*                            (TWB)                                     01 01060
C*    VERSION 7(13) 03-Aug-93 Changed environment variable for VAX      01 01070
C*                              version. (TWB)                          01 01080
C*    VERSION 7(14) 03-Sep-93 Commented out IF statement after call to  01 01090
C*                              WECARD. For EC only this resulted in    01 01100
C*                              the last calculated LOGFn and LOGFnT to 01 01110
C*                              be reported and values on E record to   01 01120
C*                              not be modified. The error was          01 01130
C*                              introduced in version 7(11) [17-Feb-93].01 01140
C*    Version 7(15) 25-Mar-96 a. Outputing a blank "S B" card when      01 01150
C*                              linked with the latest version of       01 01160
C*                              NSDFLIB - caused by a correction in the 01 01170
C*                              CNVU2S routine in NSDFLIB 1.4 -         01 01180
C*                              corrected all calls for format 2 of     01 01190
C*                              CNVU2S by LOGFT                         01 01200
C*                            b. Corrected problem when blank T1/2 field01 01210
C*                              on P record                             01 01220
C*                            c. Cleaned out redundant coding in WECARD 01 01230
C*                              by introducing ADDFRAC subroutine       01 01240
C*                            d. Other minor output cleanup             01 01250
C*                            e. Corrected problems noted by AIX XL     01 01260
C*                              Fortran Compiler 0.3.02.0002.0000       01 01270
C*    Version 7.15a 13-Apr-99 a. Check for and skip Ionized Atom        01 01280
C*                              datasets                                01 01290
C*                            b. Y2K compliance                         01 01300
C*                            c. Corrected an arithmetic overflow in    01 01310
C*                              calculating dtp                         01 01320
C*                            d. Check uniqueness. If not allowed or    01 01330
C*                              1U or 2U, added warning to report and   01 01340
C*                              terminal output and comment to new file 01 01350
C*    Version 7.2    7-Feb-01 Added code to enable program to run on    01 01360
C*                            Linux OS using GNU f77. Save statements   01 01370
C*                            were added and NBBR was initialized.      01 01380
C*    Version 7.2a  20-Mar-01 Change path for LOGFT.DAT to logical      01 01390
C*                                                                      01 01400
C*    REFER ALL COMMENTS AND INQUIRIES TO                               01 01410
C*    NATIONAL NUCLEAR DATA CENTER                                      01 01420
C*    BUILDING 197D                                                     01 01430
C*    BROOKHAVEN NATIONAL LABORATORY                                    01 01440
C*    UPTON, NEW YORK 11973                                             01 01450
C*    TELEPHONE 631-344-2901                                            01 01460
C*                                                                      01 01470
C***********************************************************************01 01480
C*                                                                      01 01490
C*                                                                      01 01500
C  PROGRAM LOGFT FOR DATA-BANK DATA SETS.                               01 01510
C                                                                       01 01520
C  THIS PROGRAM CALCULATES LOG FT FOR BETA DECAY.                       01 01530
C  FOR CAPTURE DECAY IT ALSO CALCULATES L/K RATIO.                      01 01540
C  FOR POSITRON DECAY IT ALSO CALCULATES E/B+ RATIO.                    01 01550
C  IT WILL DO SPECIAL CALCULATIONS FOR FIRST AND SECOND                 01 01560
C  FORBIDDEN UNIQUE.                                                    01 01570
C  ALL OTHER CATEGORIES ARE TREATED AS ALLOWED.                         01 01580
C                                                                       01 01590
C  THE PROGRAM WAS CONVERTED FROM FORTRAN IV TO 77.                     01 01600
C                                                                       01 01610
C  THE FOLLOWING SUBROUTINES ARE SUPPLIED WITH THE MAIN PROGRAM         01 01620
C                                                                       01 01630
C    SETUP   SETS UP SOME OF THE VALUES USED IN INTEGRATION             01 01640
C    F       THE INTEGRAND                                              01 01650
C    GAMMA   GAMMA FUNCTION (COMPLEX ARGUMENT)                          01 01660
C    CAPTUR  COMPUTES ELECTRON CAPTURE                                  01 01670
C    READWV  READS IN K, L WAVE FUNCTIONS                               01 01680
C    WAVEFN  RETURNS WAVE FUNCTION TO CAPTUR                            01 01690
C                                                                       01 01700
      PROGRAM LOGFT                                                     01 01710
C                                                                       01 01720
      INTEGER NF                                                        01 01730
      COMMON /FCOMM/NF                                                  01 01740
C                                                                       01 01750
      REAL EPS,EPS1                                                     01 01760
      COMMON/PREC1/EPS,EPS1                                             01 01770
C                                                                       01 01780
      COMMON /CARDIM/CARD,CARD2,STARS                                   01 01790
      CHARACTER*80  CARD                                                01 01800
      CHARACTER*80  CARD2                                               01 01810
      CHARACTER* 8  STARS                                               01 01820
C                                                                       01 01830
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          01 01840
     1    IBONLY,ZERUNC,EK,NUMP                                         01 01850
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             01 01860
      REAL    DPERC, DPINT,DEINT                                        01 01870
      REAL    PERC, PINT, EINT                                          01 01880
      REAL    EK(5)                                                     01 01890
C                                                                       01 01900
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              01 01910
      CHARACTER* 2  TYPE                                                01 01920
      CHARACTER     ATI*10,  DATI*2                                     01 01930
      CHARACTER*2   UNCERT                                              01 01940
C                                                                       01 01950
      COMMON /WENUM/ AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM,     01 01960
     1    FCLP, FCMNM, FCMNO, FCMNP,NPOS,ETOP                           01 01970
      INTEGER NPOS                                                      01 01980
      REAL   AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM, FCLP,       01 01990
     1    FCMNM, FCMNO, FCMNP,ETOP                                      01 02000
C                                                                       01 02010
C-REAL VARIABLES.                                                       01 02020
C                                                                       01 02030
      REAL A, AREA1U                                                    01 02040
      REAL BR                                                           01 02050
      REAL CAPTUR                                                       01 02060
      REAL DBR, DE(5), DEBAR, DEE,  DEIS,                               01 02070
     1    DELEV, DELF, DEK(5), DETOP, DFLT,                             01 02080
     2    DLAREA, DLFT, DNB, DQ(5), DT(5), DTP,DW0                      01 02090
      REAL E(5), EAR1U, EAREA, EAREAM, EAREAP, EBAR, EBARM,             01 02100
     1    EBARP, EBARR, EIS, EKTOP,                                     01 02110
     2    ELEV, ELMASS,                                                 01 02120
     3    ETOPL, ETOPM, ETOPP                                           01 02130
      REAL FC, FC1,                                                     01 02140
     1    FCM, FCMN1, FCP, FLOG, FLOGF,                                 01 02150
     2    FLOGM, FLOGP, FLOGT, FT, FTOUT                                01 02160
      REAL NB                                                           01 02170
      REAL Q(5)                                                         01 02180
      REAL THALF(5), TMULT, TP, TUL(11)                                 01 02190
      REAL W0                                                           01 02200
      REAL ZL, NBBR, DNBBR                                              01 02210
      REAL PINTM,PINTP                                                  01 02220
C                                                                       01 02230
C-External functions                                                    01 02240
      INTEGER Indexf,Iprec,LENSTR,Rlscn,TYPSTR                          01 02250
      REAL FNEW,Valstr,XF                                               01 02260
      EXTERNAL FNEW,Indexf,Iprec,LENSTR,Rlscn,TYPSTR,Valstr,XF          01 02270
C-Intrinsic functions                                                   01 02280
      Integer IABS,INDEX,INT,MAX0                                       01 02290
      Real ALOG10                                                       01 02300
      Intrinsic ALOG10,IABS,INDEX,INT,MAX0                              01 02310
C                                                                       01 02320
C-INTEGER VARIABLES.                                                    01 02330
C                                                                       01 02340
      INTEGER I, IE, IK, IPRINT, ISKIP,                                 01 02350
     1    ISKIP1,iskip2,lprec                                           01 02360
      INTEGER J                                                         01 02370
      INTEGER INUMP                                                     01 02380
C                                                                       01 02390
C-STRING VARIABLES.                                                     01 02400
C                                                                       01 02410
      CHARACTER* 1  C6                                                  01 02420
      CHARACTER* 3  C79                                                 01 02430
      CHARACTER* 1  C8                                                  01 02440
      CHARACTER*80  CARDI                                               01 02450
      CHARACTER*80  LINE                                                01 02460
      Character*80 crdcom                                               01 02470
      CHARACTER*10  STA                                                 01 02480
      CHARACTER* 6  STB                                                 01 02490
      CHARACTER* 5  STR                                                 01 02500
      CHARACTER* 2  UT(11)                                              01 02510
      CHARACTER*11   XDATE                                              01 02520
      CHARACTER     AN(5)*12, DAN(5)*2                                  01 02530
C                                                                       01 02540
      LOGICAL       PDONE,FIRSTI,DONBBR,somskp,levfnd                   01 02550
      Logical dowarn                                                    01 02560
C                                                                       01 02570
C    UNCERT    REMEMBERS THE NONNUMERICAL UNCERTAINTY OF INPUT          01 02580
C    PDONE     END OF PARENT RECORD FOR THE DATASET                     01 02590
C    NUMP      NO OF PARENT RECORDS                                     01 02600
C                                                                       01 02610
C-INITIALIZATION SECTION.                                               01 02620
C                                                                       01 02630
      DATA TUL/31536000.,86400.,3600.,60., 1., 1., 1., 1., 1., 1., 1./  01 02640
C     Right justify single character symbols to allow for possibility   01 02650
C       of symbol being last character in T field (TWB 921224)          01 02660
      DATA UT /' Y',     ' D',  ' H', ' M',' S','MS','US','NS','PS',    01 02670
     1         'FS','AS'/                                               01 02680
      DATA ELMASS, ZL/0.511004, 1./                                     01 02690
C                                                                       01 02700
 9000 FORMAT(A)                                                         01 02710
 9001 FORMAT('1'/98X, A)                                                01 02720
 9002 FORMAT(1X, A)                                                     01 02730
 9004 FORMAT('+', 100X,A)                                               01 02740
 9005 FORMAT('0', 20X,A )                                               01 02750
 9008 FORMAT(20X, A)                                                    01 02760
 9009 FORMAT('0', 8X, 'TRANSITION(KEV)=', A, 1X, A,                     01 02770
     1    ', T1/2(SEC)=', A, 1X, A, ', BETA BRANCHING(%)=',             01 02780
     2    A, 1X, A, ', PARTIAL T1/2(SEC)=', A, 1X, A)                   01 02790
 9010 FORMAT('0', 8X, 'TRANSITION(KEV)=', A, 1X, A,                     01 02800
     1    ', T1/2(SEC)=', A, 1X, A, ', BRANCHING(%)=',                  01 02810
     2    A, 1X, A, ', PARTIAL T1/2(SEC)=', A, 1X, A)                   01 02820
 9011 FORMAT(20X,A , A, 1X, A)                                          01 02830
 9012 FORMAT(45X,A )                                                    01 02840
 9013 FORMAT(20X, A, I1, A, F6.3,A)                                     01 02850
 9016 FORMAT (20X, A, F7.3)                                             01 02860
 9017 FORMAT(20X, 'CAPTURE TO POSITRON RATIO =', 1PE10.3, '+-', E9.2,   01 02870
     1    10X, 'LOG(E/B+)=', 0PF7.3, 10X, 'K/B+=', 1PE10.3)             01 02880
 9018 FORMAT(20X, 'POSITRON INTENSITY = ', 1PE8.2, '+-', 1PE9.1, ' ,',  01 02890
     1    10X, 'ELECTRON CAPTURE INTENSITY =', E9.2, '+-', E9.1, ' ,')  01 02900
 9020 FORMAT(20X, 'E=', F8.2, 5X, 'LOG F', I1, '=', F6.3, '+-', F6.3)   01 02910
 9021 FORMAT(' LOG F', I1, 'T = ', F6.3, '+-', F6.3, 5X, 'F', I1, 'T=', 01 02920
     1    E12.5)                                                        01 02930
 9022 FORMAT('+', 50X, 'AVERAGE BETA(+-) ENERGY=', F8.2, '+-', F7.3,    01 02940
     1    4X, 'EBAR/E = ', F7.4, /)                                     01 02950
 9023 FORMAT('0', A, 5X, A)                                             01 02960
 9024 FORMAT('+', 85X, A)                                               01 02970
 9025 FORMAT(1X, A, 10X, A)                                             01 02980
 9026 FORMAT(////,98X,A)                                                01 02990
C                                                                       01 03000
      stars='********'                                                  01 03010
      crdcom='*****'                                                    01 03020
      dowarn=.FALSE.                                                    01 03030
      nbbr=0.                                                           01 03040
C                                                                       01 03050
      Write(6,9000)' LOGFT Version 7.2a [20-Mar-01]'                    01 03060
C                                                                       01 03070
C+++MDC+++                                                              01 03080
C...ANS                                                                 01 03090
C/99901 FORMAT(' INPUT DATA SET FILE (TEST.DAT):    ')                  01 03100
C/99902 FORMAT(' OUTPUT REPORT FILE (LOGFT.RPT):     ')                 01 03110
C/99903 FORMAT(' DATA TABLE (LOGFT.DAT): ')                             01 03120
C/99904 FORMAT(' OUTPUT DATA SET FILE (LOGFT.NEW):   ')                 01 03130
C...VAX, DVF, UNX                                                       01 03140
99901 FORMAT(' INPUT DATA SET FILE (data.tst):    ',$)                  01 03150
99902 FORMAT(' OUTPUT REPORT FILE (logft.rpt):     ',$)                 01 03160
99903 FORMAT(' DATA TABLE (logft.dat): ',$)                             01 03170
99904 FORMAT(' OUTPUT DATA SET FILE (logft.new):   ',$)                 01 03180
C---MDC                                                                 01 03190
99900 FORMAT(A)                                                         01 03200
C+++MDC                                                                 01 03210
C...ANS, UNX                                                            01 03220
C/      WRITE(6,99901)                                                  01 03230
C/      READ(5,99900) LINE                                              01 03240
C/      IF(LINE.EQ.' ') LINE='data.tst'                                 01 03250
C/      OPEN(UNIT=35, ACCESS='SEQUENTIAL' ,STATUS='OLD',                01 03260
C/     1     FILE=LINE)                                                 01 03270
C/      WRITE(6,99902)                                                  01 03280
C/      READ(5,99900) LINE                                              01 03290
C/      IF(LINE.EQ.' ') LINE='logft.rpt'                                01 03300
C/      OPEN(UNIT=36, ACCESS='SEQUENTIAL', STATUS='UNKNOWN',            01 03310
C/     1     FILE=LINE)                                                 01 03320
C/      WRITE(6,99903)                                                  01 03330
C/      READ(5,99900) LINE                                              01 03340
C/      IF(LINE.EQ.' ') LINE='logft.dat'                                01 03350
C/      OPEN(UNIT=20, ACCESS='SEQUENTIAL' ,STATUS='OLD',                01 03360
C/     1     FILE=LINE)                                                 01 03370
C/      WRITE(6,99904)                                                  01 03380
C/      READ(5,99900) LINE                                              01 03390
C/      IF(LINE.EQ.' ') LINE='logft.new'                                01 03400
C/      OPEN(UNIT=21, ACCESS='SEQUENTIAL' ,STATUS='UNKNOWN',            01 03410
C/     1     FILE=LINE)                                                 01 03420
C...VAX, DVF                                                            01 03430
      WRITE(6,99901)                                                    01 03440
      READ(5,99900) LINE                                                01 03450
      IF(LINE.EQ.' ') LINE='DATA.TST'                                   01 03460
      OPEN(UNIT=35, ACCESS='SEQUENTIAL' ,STATUS='OLD',READONLY,         01 03470
     1     FILE=LINE)                                                   01 03480
      WRITE(6,99902)                                                    01 03490
      READ(5,99900) LINE                                                01 03500
      IF(LINE.EQ.' ') LINE='LOGFT.RPT'                                  01 03510
      OPEN(UNIT=36, ACCESS='SEQUENTIAL', STATUS='UNKNOWN',              01 03520
     1     FILE=LINE)                                                   01 03530
      WRITE(6,99903)                                                    01 03540
      READ(5,99900) LINE                                                01 03550
C...DVF                                                                 01 03560
      IF(LINE.EQ.' ') LINE='LOGFT.DAT'                                  01 03570
C...VAX                                                                 01 03580
C/      IF(LINE.EQ.' ') LINE='ANALENSDF:LOGFT.DAT'                      01 03590
C...VAX, DVF                                                            01 03600
      OPEN(UNIT=20, ACCESS='SEQUENTIAL' ,STATUS='OLD', READONLY,        01 03610
     1     FILE=LINE)                                                   01 03620
      WRITE(6,99904)                                                    01 03630
      READ(5,99900) LINE                                                01 03640
      IF(LINE.EQ.' ') LINE='LOGFT.NEW'                                  01 03650
      OPEN(UNIT=21, ACCESS='SEQUENTIAL' ,STATUS='UNKNOWN',              01 03660
     1     CARRIAGECONTROL='LIST', FILE=LINE)                           01 03670
C---MDC---                                                              01 03680
C                                                                       01 03690
      XDATE=' '                                                         01 03700
C+++MDC+++                                                              01 03710
C...DVF, VAX, UNX                                                       01 03720
      CALL DATE_20(XDATE)                                               01 03730
C...ANS                                                                 01 03740
C---MDC---                                                              01 03750
C                                                                       01 03760
C-PROGRAM BEGINS BY READING WAVE FUNCTION CARDS IN READWV.              01 03770
C-THE LAST OF THESE CONTAINS BLANKS.                                    01 03780
C                                                                       01 03790
      CALL READWV                                                       01 03800
C                                                                       01 03810
C-NTRY IS THE NUMBER IF ITERATIONS TO USE IN THE INTEGRATION BEFORE     01 03820
C-GIVING UP.                                                            01 03830
C-PREC IS THE PRECISION DESIRED.                                        01 03840
C                                                                       01 03850
C    Precision of 0.000001 is too strict for very low W. Old version    01 03860
C      of LOGFT using SIMCON used 0.0001 so comprimise                  01 03870
C      EPS=1.0E-6                                                       01 03880
C      EPS1=100.*EPS                                                    01 03890
C     Changed from EPS=1.E-5, EPS1=10.*EPS to EPS=5.E-6,EPS1=EPS        01 03900
C       (TWB. 921216)                                                   01 03910
      EPS=5.0E-6                                                        01 03920
      EPS1=0.1*EPS                                                      01 03930
C      PREC = 1.0E-6                                                    01 03940
      WRITE(36, 1999)                                                   01 03950
 1999 FORMAT('1PROGRAM  L O G F T  VERSION 7.2a AS OF 20-Mar-2001.')    01 03960
      IPRINT = 1                                                        01 03970
      INUMP=0                                                           01 03980
      FIRSTI=.TRUE.                                                     01 03990
      iskip=0                                                           01 04000
      somskp=.FALSE.                                                    01 04010
      levfnd=.FALSE.                                                    01 04020
C                                                                       01 04030
C-CARD FORMAT.                                                          01 04040
C                                                                       01 04050
  100 READ(35, 9000, END=1265) CARD                                     01 04060
      I = TYPSTR(CARD(1:1))                                             01 04070
      IF (I .LT. 0 .OR. I .EQ. 2) GO TO 900                             01 04080
      C6=CARD(6:6)                                                      01 04090
      C79=CARD(7:9)                                                     01 04100
      IF (C6 .NE. ' ' .AND. C6 .NE. '1') GO TO 600                      01 04110
      C8=CARD(8:8)                                                      01 04120
C     "PN" record was never being read since check was only on column   01 04130
C       7 being blank (TWB. 930104)                                     01 04140
      IF(CARD(7:7) .NE. ' ')THEN                                        01 04150
         IF(CARD(7:8) .NE. 'PN')GO TO 900                               01 04160
      ENDIF                                                             01 04170
      IF(CARD(1:3) .EQ. '   ') GO TO 900                                01 04180
C                                                                       01 04190
C   I CARD                                                              01 04200
C   ======                                                              01 04210
      IF(C79 .EQ. '   ') THEN                                           01 04220
         BR = 1.                                                        01 04230
         DBR = 0.                                                       01 04240
         NB = 1.                                                        01 04250
         DNB = 0.                                                       01 04260
         NUMP=1                                                         01 04270
         PDONE=.FALSE.                                                  01 04280
C        Added warning message to terminal output when data skipped     01 04290
C          (TWB. 930212)                                                01 04300
         If(.NOT.firsti)Then                                            01 04310
            If(iskip .gt. 0)Then                                        01 04320
               Write(6,9002)'***** Data '//                             01 04330
     2           'set will not be modified - See LOGFT report'          01 04340
            Else                                                        01 04350
               If(somskp)Write(6,9002)'***** Some records '//           01 04360
     2           'will not be modified - See LOGFT report'              01 04370
            Endif                                                       01 04380
         Endif                                                          01 04390
         iskip=0                                                        01 04400
         somskp=.FALSE.                                                 01 04410
         levfnd=.FALSE.                                                 01 04420
         IF(FIRSTI) THEN                                                01 04430
            WRITE(36,9026) XDATE                                        01 04440
            FIRSTI=.FALSE.                                              01 04450
         ELSE                                                           01 04460
            WRITE(36, 9001) XDATE                                       01 04470
         ENDIF                                                          01 04480
         WRITE(36, 9002) CARD                                           01 04490
C        Added terminal output of DSID so that messages would be in     01 04500
C          context (TWB. 930212)                                        01 04510
         Write(6,9002)'Processing=====>'//card(1:39)                    01 04520
C        Check for Ionized Atom dataset and skip                        01 04530
         If(INDEX(card(10:39),' DECAY').GT.0 .AND.                      01 04540
     2     (INDEX(card(10:39),'[').GT.0                                 01 04550
     3     .AND. INDEX(card(10:39),'[')                                 01 04560
     4     .LT.INDEX(card(10:39),' DECAY')))Then                        01 04570
            Write(36,9002)'***** Ionized Atom dataset - skipping'       01 04580
            iskip=1                                                     01 04590
         EndIf                                                          01 04600
         STR=CARD(1:5)                                                  01 04610
         i=Rlscn(str,1,A)                                               01 04620
         If(i .EQ. 6)Then                                               01 04630
            If(INDEX(str(1:3),' ') .NE. 0)Then                          01 04640
               Write(36,9002)'***** Error in NUCID - Z=0 set'           01 04650
               iskip=1                                                  01 04660
            Else                                                        01 04670
               A=Valstr(str(1:3))                                       01 04680
            Endif                                                       01 04690
         Else If(i .NE. 4)Then                                          01 04700
            Write(36,9002)'***** Error in NUCID - Z=0 set'              01 04710
            iskip=1                                                     01 04720
         Else                                                           01 04730
            CALL IZEL(STR(4:5),IZ)                                      01 04740
            If(iz .EQ. -1)Then                                          01 04750
               Write(36,9002)'***** Element not identified - Z=0 set'   01 04760
               iskip=1                                                  01 04770
            Endif                                                       01 04780
         Endif                                                          01 04790
         If(iskip .EQ. 1)iz=0                                           01 04800
         DO 120 I=1,5                                                   01 04810
            Q(I) = 0.                                                   01 04820
  120    CONTINUE                                                       01 04830
         GOTO 900                                                       01 04840
      ENDIF                                                             01 04850
C                                                                       01 04860
C   COLUMNS 8,9, HAVE SOME LETTERS                                      01 04870
C                                                                       01 04880
C    P CARD                                                             01 04890
C    ======                                                             01 04900
C                                                                       01 04910
      IF (C8 .EQ.'P') THEN                                              01 04920
C                                                                       01 04930
C        ISKIP value reflects that of last p card if there are multiple 01 04940
C                                                                       01 04950
         WRITE(36, 9002) CARD                                           01 04960
         STA=CARD(10:19)                                                01 04970
         STB=CARD(20:21)                                                01 04980
         CALL CNVS2U(STA,STB,EIS,DEIS)                                  01 04990
C        Added check for non-numeric parent energies (TWB. 930212)      01 05000
         Call Lbsup(sta)                                                01 05010
         Call Lbsup(stb)                                                01 05020
         If(Typstr(sta) .EQ. 2 .OR. Typstr(sta) .EQ. -1                 01 05030
     2     .OR. Typstr(stb) .EQ. 2 .OR. Typstr(stb) .EQ. -1)Then        01 05040
            Write(36,9002)'Parent energy value uncertain - skip output' 01 05050
            iskip=1                                                     01 05060
         Endif                                                          01 05070
         STA=CARD(65:74)                                                01 05080
         STB=CARD(75:76)                                                01 05090
         CALL CNVS2U(STA,STB,Q(NUMP),DQ(NUMP))                          01 05100
C                                                                       01 05110
C   if DQP (DQ) field has non numeric uncertainty, do not output new rec01 05120
C                                                                       01 05130
         IF(STB .NE. ' ' .AND. DQ(NUMP).EQ.0.)THEN                      01 05140
            ISKIP=1                                                     01 05150
            WRITE(36,9002)                                              01 05160
     1          ' DQP on P-card with non numeric uncert. - skip output' 01 05170
         ENDIF                                                          01 05180
         Q(NUMP) = Q(NUMP) + EIS                                        01 05190
         DQ(NUMP) = SQRT(DQ(NUMP) * DQ(NUMP) + DEIS * DEIS)             01 05200
         STA=CARD(40:49)                                                01 05210
         STB=CARD(50:55)                                                01 05220
C                                                                       01 05230
         Call Lbsup(sta)                                                01 05240
         I=LENSTR(STA)                                                  01 05250
C        Check for blank T1/2 field                                     01 05260
         If(i .LE. 0)Then                                               01 05270
            Write(6,9002)'No T1/2 given. 1 S assumed'                   01 05280
            Write(36,9004)'NO T1/2 GIVEN. *******'                      01 05290
            Write(36,FMT='(101X,A)')'1 S ASSUMED'                       01 05300
            sta='1 S'                                                   01 05310
            i=3                                                         01 05320
            iskip=1                                                     01 05330
         EndIf                                                          01 05340
C        Format manual allows "?" after half-life (TWB. 921224)         01 05350
         IF(STA(I:I) .EQ. '?')THEN                                      01 05360
            If(i .GT. 1)Then                                            01 05370
               WRITE(6,9002)'NOTE UNCERTAIN T1/2 ASSIGNMENT'            01 05380
               WRITE(36, 9004) 'NOTE T1/2 ASSIGNMENT? *******'          01 05390
               I=LENSTR(STA(1:I-1))                                     01 05400
            Else                                                        01 05410
C              Check for missing T1/2                                   01 05420
               Write(6,9002)'No T1/2 given. 1 S assumed'                01 05430
               Write(36,9004)'NO T1/2 GIVEN. *******'                   01 05440
               Write(36,FMT='(101X,A)')'1 S ASSUMED'                    01 05450
               sta='1 S'                                                01 05460
               iskip=1                                                  01 05470
            EndIf                                                       01 05480
         ENDIF                                                          01 05490
C        Added error messages to report file                            01 05500
         IF(STA(I:I).LT.'A' .AND. STA(I:I).GT.'Z') THEN                 01 05510
            WRITE(6,9002)                                               01 05520
     1           'NO UNITS GIVEN FOR LIFETIME--SECONDS ASSUMED'         01 05530
            WRITE(36, 9004) 'NO UNITS FOR T1/2--   *******'             01 05540
            WRITE(36,FMT='(101X,A)')'SECONDS ASSUMED'                   01 05550
            IK=5                                                        01 05560
            ISKIP=1                                                     01 05570
         ELSE                                                           01 05580
            DO 130 IK=11,1,-1                                           01 05590
               IF(INDEX(STA,UT(IK)).NE.0) GO TO 140                     01 05600
  130       CONTINUE                                                    01 05610
            WRITE(6,9002)                                               01 05620
     1           'HALF-LIFE UNIT NOT RECOGNIZED. SECONDS SUBSTITUTED'   01 05630
            WRITE(36, 9004) 'T1/2 UNIT NOT RECOGNIZED *****'            01 05640
            WRITE(36,FMT='(101X,A)')'SECONDS SUBSTITUTED'               01 05650
            ISKIP=1                                                     01 05660
            IK=5                                                        01 05670
         ENDIF                                                          01 05680
C                                                                       01 05690
  140    CALL CNVS2U(STA, STB, THALF(NUMP), DT(NUMP))                   01 05700
         TMULT=1.                                                       01 05710
         IF(IK.GT.5) TMULT=.001**(IK-5)                                 01 05720
         THALF(NUMP) = THALF(NUMP) * TUL(IK) * TMULT                    01 05730
         DT(NUMP) = DT(NUMP) * TUL(IK) * TMULT                          01 05740
         NUMP=NUMP+1                                                    01 05750
         GOTO 900                                                       01 05760
      ENDIF                                                             01 05770
C                                                                       01 05780
C-PICK UP BRANCHING RATIO INFORMATION FROM N-CARD.                      01 05790
C                                          ======                       01 05800
C   PN card has P in column 7                                           01 05810
C                                                                       01 05820
      IF(C8 .EQ.'N') THEN                                               01 05830
         IF(CARD(7:7) .EQ. 'P') THEN                                    01 05840
C                                                                       01 05850
C   PN CARD                                                             01 05860
C   =======                                                             01 05870
C                                                                       01 05880
            STA=CARD(42:49)                                             01 05890
C           Added cross check between NB*BR and NBBR (TWB. 930212)      01 05900
            IF(STA.NE.' ') THEN                                         01 05910
               STB=CARD(50:55)                                          01 05920
               CALL CNVS2U(STA, STB, NBBR,DNBBR)                        01 05930
               WRITE(36, 9002) CARD                                     01 05940
               IF(ABS(NBBR-NB*BR) .GT. 0.001)WRITE(36,FMT=              01 05950
     2           '(''+'',100X,''CHECK NB*BR='',F5.3,'' .NE. NBBR *'')') 01 05960
     4           NB*BR                                                  01 05970
            ELSE                                                        01 05980
               IF(BR .NE. 1. .AND.(ABS(NB*BR-1.) .GT. 0.001))           01 05990
     2           WRITE(36,FMT=                                          01 06000
     3           '(''+'',100X,''CHECK NB*BR='',F5.3,''     *******'')') 01 06010
     4           NB*BR                                                  01 06020
               WRITE(36, 9002) CARD                                     01 06030
            ENDIF                                                       01 06040
            DONBBR=.FALSE.                                              01 06050
         ELSE                                                           01 06060
C                                                                       01 06070
C   N CARD                                                              01 06080
C                                                                       01 06090
C    BR field                                                           01 06100
C                                                                       01 06110
            WRITE(36, 9002) CARD                                        01 06120
            STA=CARD(32:39)                                             01 06130
            IF (STA .NE. ' ') THEN                                      01 06140
                STB=CARD(40:41)                                         01 06150
                CALL CNVS2U(STA, STB, BR, DBR)                          01 06160
            ELSE                                                        01 06170
               BR=0.                                                    01 06180
               DBR=0.                                                   01 06190
            ENDIF                                                       01 06200
            IF(BR .EQ. 0.)THEN                                          01 06210
               WRITE(36,9002)                                           01 06220
     1             ' BR=0. reset to 1. - skip output'                   01 06230
               ISKIP=1                                                  01 06240
               BR=1.0                                                   01 06250
            ENDIF                                                       01 06260
            STA=CARD(42:49)                                             01 06270
C                                                                       01 06280
C    NB field                                                           01 06290
C                                                                       01 06300
            IF (STA .EQ. ' ') THEN                                      01 06310
                NB = 1. / BR                                            01 06320
C              Changed from DBR = 0. to DNB = 0. and added warning      01 06330
C                message (TWB. 921224)                                  01 06340
                DNB = 0.                                                01 06350
                WRITE(36, 9004) 'NB ASSUMED 1/BR       *******'         01 06360
                GOTO 900                                                01 06370
            ENDIF                                                       01 06380
C                                                                       01 06390
C-IF STRING 'NR' IS IN NB FIELD, THEN ASSUME SAME                       01 06400
C-NORMALIZATION AS FOR GAMMAS.                                          01 06410
C                                                                       01 06420
            CALL SQZSTR(STA, ' ' )                                      01 06430
            IF (STA(1:2) .EQ. 'NR') THEN                                01 06440
C              Not allowed in manual (TWB. 921224)                      01 06450
               WRITE(36, 9004) '"NR" IN NB NOT ALLOWED ******'          01 06460
                STA=CARD(22:29)                                         01 06470
                STB=CARD(30:31)                                         01 06480
            ELSE                                                        01 06490
                STB=CARD(50:51)                                         01 06500
            ENDIF                                                       01 06510
            CALL CNVS2U(STA, STB, NB, DNB)                              01 06520
            DONBBR=.TRUE.                                               01 06530
C           Replace ambiguous and misleading message with something     01 06540
C             more informative and move to after PN record (TWB. 930104)01 06550
C            WRITE(36, 9004) 'NOTE BRANCHING RATIO  *******'            01 06560
         ENDIF                                                          01 06570
         GO TO 900                                                      01 06580
      ENDIF                                                             01 06590
C                                                                       01 06600
C   G,L,B OR E RECORD(NOT P)                                            01 06610
C   skip other cards                                                    01 06620
C                                                                       01 06630
      IF (C8 .NE. 'L' .AND. C8. NE. 'E' .AND.                           01 06640
     1    C8 .NE. 'B') GOTO 900                                         01 06650
C                                                                       01 06660
C  no more parent record                                                01 06670
C                                                                       01 06680
      IF(.NOT. PDONE) THEN                                              01 06690
         NUMP=NUMP-1                                                    01 06700
         PDONE=.TRUE.                                                   01 06710
      ENDIF                                                             01 06720
C  IF last Q value is 0, error message and ISKIP set                    01 06730
      IF (NUMP.EQ.0) THEN                                               01 06740
         IF (ISKIP .EQ. 0) WRITE(36, 9005)                              01 06750
     1             'Q-VALUE NOT GIVEN. MISSING P-CARD.'                 01 06760
         ISKIP = 1                                                      01 06770
         GOTO 900                                                       01 06780
      ENDIF                                                             01 06790
      IF (Q(NUMP) .EQ. 0.) THEN                                         01 06800
         IF (ISKIP .EQ. 0) WRITE(36, 9005)                              01 06810
     1             'Q-VALUE NOT GIVEN ON P-CARD.'                       01 06820
         ISKIP = 1                                                      01 06830
         GOTO 900                                                       01 06840
      ENDIF                                                             01 06850
      IF (C8 .EQ. 'L') THEN                                             01 06860
C                                                                       01 06870
C-PROCESS LEVEL RECORD. L-RECORD                                        01 06880
C                       ========                                        01 06890
C        Added cross check between NB*BR and NBBR (TWB. 930212)         01 06900
         IF(DONBBR)THEN                                                 01 06910
            IF(BR .NE. 1. .AND.(ABS(NB*BR-1.) .GT. 0.001))WRITE(36,FMT= 01 06920
     2        '(''+'',100X,''CHECK NB*BR='',F5.3,''     *******'')')    01 06930
     3        NB*BR                                                     01 06940
            DONBBR=.FALSE.                                              01 06950
         ENDIF                                                          01 06960
         INUMP=0                                                        01 06970
         ISKIP1 = 0                                                     01 06980
         iskip2=0                                                       01 06990
         levfnd=.TRUE.                                                  01 07000
         WRITE(36, 9002) CARD                                           01 07010
         STA=CARD(10:19)                                                01 07020
         STB=CARD(20:21)                                                01 07030
         CALL CNVS2U(STA, STB, ELEV, DELEV)                             01 07040
C                                                                       01 07050
C   if level energy value contains non numeric info, do not write new   01 07060
C   record                                                              01 07070
         Call Lbsup(sta)                                                01 07080
         Call Lbsup(stb)                                                01 07090
C        Program was not picking up "0+X", etc. (TWB. 930212)           01 07100
         If(Typstr(sta) .EQ. 2 .OR. Typstr(sta) .EQ. -1                 01 07110
     2     .OR. Typstr(stb) .EQ. 2 .OR. Typstr(stb) .EQ. -1)Then        01 07120
            ISKIP2=1                                                    01 07130
            somskp=.true.                                               01 07140
            WRITE(36,9002)                                              01 07150
     1        'Level energy value uncertain - skip output'              01 07160
         ENDIF                                                          01 07170
C                                                                       01 07180
C-CALCULATE BRANCH ENERGY FROM Q-VALUE.                                 01 07190
C                                                                       01 07200
         DO 200 I=1,NUMP                                                01 07210
            E(I) = Q(I) - ELEV                                          01 07220
            DE(I) = SQRT(DQ(I) * DQ(I) + DELEV * DELEV)                 01 07230
            EK(I) = E(I)                                                01 07240
            DEK(I) = DE(I)                                              01 07250
            E(I) = E(I) * 0.001                                         01 07260
            DE(I) = DE(I) * 0.001                                       01 07270
  200    CONTINUE                                                       01 07280
         GOTO 900                                                       01 07290
      ENDIF                                                             01 07300
C                                                                       01 07310
C   B or E card                                                         01 07320
C                                                                       01 07330
      CARDI=CARD                                                        01 07340
      IBONLY=0                                                          01 07350
      ZERUNC=0                                                          01 07360
      IF (C8 .EQ. 'B') THEN                                             01 07370
C                                                                       01 07380
C-START PROCESSING B RECORD.                                            01 07390
C                  ========                                             01 07400
C                                                                       01 07410
         WRITE(36, 9000) '0'                                            01 07420
         If(.NOT.levfnd)Then                                            01 07430
            Write(36,9002)card                                          01 07440
            Write(36,9002)                                              01 07450
     2        '***** Unplaced B or redundant B for level ignored'       01 07460
            iskip1=1                                                    01 07470
            somskp=.TRUE.                                               01 07480
            Goto 900                                                    01 07490
         Endif                                                          01 07500
         levfnd=.FALSE.                                                 01 07510
         IZ = -1 * IABS(IZ)                                             01 07520
         TYPE=CARD(78:79)                                               01 07530
         STA=CARD(22:29)                                                01 07540
         STB=CARD(30:31)                                                01 07550
         CALL CNVS2U(STA, STB, PERC, DPERC)                             01 07560
         UNCERT=STB                                                     01 07570
C     NOTE IF DPERC = 0.0 (MISSING UNCERT)                              01 07580
C     Added special check for 100% feeding (TWB. 930212)                01 07590
         IF ((DPERC .EQ. 0.0 .AND. PERC .NE. 100.) .OR. STB .NE. ' ')   01 07600
     2     ZERUNC = 1                                                   01 07610
         jprec=Iprec(sta)                                               01 07620
         GOTO 300                                                       01 07630
C                                                                       01 07640
C-END PROCESSING B RECORD.                                              01 07650
C                                                                       01 07660
      ELSE                                                              01 07670
C                                                                       01 07680
C-START PROCESSING E RECORD.                                            01 07690
C                  ========                                             01 07700
C                                                                       01 07710
         If(.NOT.levfnd)Then                                            01 07720
            WRITE(36, 9000) '0'                                         01 07730
            Write(36,9002)card                                          01 07740
            Write(36,9002)                                              01 07750
     2        '***** Unplaced E or redundant E for level ignored'       01 07760
            somskp=.TRUE.                                               01 07770
            iskip1=1                                                    01 07780
            Goto 900                                                    01 07790
         Endif                                                          01 07800
         levfnd=.FALSE.                                                 01 07810
         CALL PROCE                                                     01 07820
      ENDIF                                                             01 07830
C                                                                       01 07840
                                                                        01 07850
C-END PROCESSING OF E RECORD.                                           01 07860
C                                                                       01 07870
C********************                                                   01 07880
C-EITHER B- OR EC/B+ COMES THROUGH HERE.                                01 07890
C   B CARD OR E CARD                                                    01 07900
C   ======    ======                                                    01 07910
C                                                                       01 07920
C   do the calculations below once for each parent record               01 07930
C                                                                       01 07940
  300 Continue                                                          01 07950
      INUMP=INUMP+1                                                     01 07960
      CARD2=' '                                                         01 07970
      ETOP = 0.                                                         01 07980
      DETOP = 0.                                                        01 07990
      ISKIP1=iskip2                                                     01 08000
      If(e(inump) .LE. 0.0)Then                                         01 08010
         Write(36,                                                      01 08020
     2     FMT='(''0'', 8X, ''TRANSITION(KEV)='', E8.2, 1X, E8.2,       01 08030
     2       '' less than or equal to 0 - Skipping calculation'')')     01 08040
     2     E(inump),DE(inump)                                           01 08050
         Write(36, 9023) cardi, 'OLD CARD'                              01 08060
         Write(36, 9024) 'OLD CARD KEPT'                                01 08070
         Write(21, 9000) cardi                                          01 08080
         somskp=.TRUE.                                                  01 08090
         iskip1=1                                                       01 08100
         IF(INUMP.LT.NUMP) GO TO 300                                    01 08110
         Goto 100                                                       01 08120
      Endif                                                             01 08130
      IF (PERC .LE. 0.) THEN                                            01 08140
C                                                                       01 08150
C-RECOVERY FROM MISSING BRANCHING RATIO.                                01 08160
C                                                                       01 08170
          WRITE(36, 9002)'  INTENSITY SET AT 100.'                      01 08180
          PERC = 100.                                                   01 08190
          EINT = PERC                                                   01 08200
          DEINT = 0.                                                    01 08210
          ISKIP1 = 1                                                    01 08220
          somskp=.TRUE.                                                 01 08230
      ENDIF                                                             01 08240
C                                                                       01 08250
C-CONVERT ENERGY TO MC ** 2 UNITS AND ADD REST MASS.                    01 08260
C                                                                       01 08270
      W0 = E(INUMP) / ELMASS + ZL                                       01 08280
      DW0 = DE(INUMP) / ELMASS                                          01 08290
C                                                                       01 08300
C-SET FLAG INITIALLY FOR ALLOWED TRANSITION.                            01 08310
C                                                                       01 08320
      IE = 0                                                            01 08330
C                                                                       01 08340
C-TEST IF FIRST FORBIDDEN UNIQUE OR SECOND FORBIDDEN UNIQUE.            01 08350
C                                                                       01 08360
      IF (TYPE .EQ. '1U') IE = 1                                        01 08370
      IF (TYPE .EQ. '2U') IE = 2                                        01 08380
C                                                                       01 08390
C-Check for other types which will be assumed to be allowed             01 08400
C                                                                       01 08410
      If(type.NE.'  ' .AND. ie.EQ.0)Then                                01 08420
         dowarn=.TRUE.                                                  01 08430
      Else                                                              01 08440
         dowarn=.FALSE.                                                 01 08450
         crdcom='*****'                                                 01 08460
      EndIf                                                             01 08470
C                                                                       01 08480
C-HALF-LIFE.                                                            01 08490
C                                                                       01 08500
      IF (THALF(INUMP) .EQ. 0.) THEN                                    01 08510
         WRITE(36, 9008)'HALF LIFE SET TO 1 S'                          01 08520
         ISKIP1 = 1                                                     01 08530
         somskp=.TRUE.                                                  01 08540
         THALF(INUMP) = 1.                                              01 08550
         DT(INUMP) = 0.                                                 01 08560
         FLOGT = 0.                                                     01 08570
      ENDIF                                                             01 08580
      IF(NBBR.EQ.0.) THEN                                               01 08590
         TP = 100. * THALF(INUMP) / ABS(PERC) / BR / NB                 01 08600
      ELSE                                                              01 08610
         TP= 100. * THALF(INUMP) /ABS(PERC) / NBBR                      01 08620
      ENDIF                                                             01 08630
      DTP = 0.                                                          01 08640
      IF (THALF(INUMP) .NE. 0.) THEN                                    01 08650
         dtp=dt(inump)/thalf(inump)                                     01 08660
         dtp=dtp*dtp                                                    01 08670
         IF(NBBR.EQ.0.) THEN                                            01 08680
            dtp=dtp+(dbr/br)*(dbr/br)+(dnb/nb)*(dnb/nb)                 01 08690
         ELSE                                                           01 08700
            dtp=dtp+(dnbbr/nbbr)*(dnbbr/nbbr)                           01 08710
         ENDIF                                                          01 08720
      ENDIF                                                             01 08730
      IF (PERC .NE. 0.) DTP = DTP + DPERC * DPERC / PERC /PERC          01 08740
      IF (DTP .NE. 0.) DTP = TP * SQRT(DTP)                             01 08750
C-LOG(E) = 0.43429                                                      01 08760
      FLOGT = ALOG10(TP)                                                01 08770
      DFLT = DTP * 0.43429 / TP                                         01 08780
C                                                                       01 08790
      CALL CNVU2S(EK(INUMP), DEK(INUMP), AN(1), 8, DAN(1), 2)           01 08800
      CALL CNVU2S(THALF(INUMP), DT(INUMP), AN(2), 12, DAN(2), 2)        01 08810
C     Corrected for overflow on branching ratio (TWB. 930212)           01 08820
      If(dperc .NE. 0)Then                                              01 08830
         CALL CNVU2S(PERC, DPERC, AN(3), 8, DAN(3), 2)                  01 08840
      Else                                                              01 08850
         lprec=-2                                                       01 08860
         If(ALOG10(perc) .GE. 1.)lprec=lprec-INT(ALOG10(perc))+1        01 08870
         Call Cnvu2s(perc,0.0,an(3),8,dan(3),lprec)                     01 08880
         dan(3)=uncert                                                  01 08890
      Endif                                                             01 08900
      CALL CNVU2S(TP, DTP, AN(4), 12, DAN(4), 2)                        01 08910
C                                                                       01 08920
C-IZ < 0 FOR B- DECAY.                                                  01 08930
C                                                                       01 08940
      IF(IZ.GT.0 .AND. IBONLY.NE.0) THEN                                01 08950
C                                                                       01 08960
C-ONLY POSITRON INTENSITY WAS GIVEN.                                    01 08970
C-EC INTENSITY WILL BE CALCULATED.                                      01 08980
C                                                                       01 08990
          WRITE(36, 9009)                                               01 09000
     1    (   AN(J)(1:8) ,DAN(J)(1:2) ,AN(J+1) ,DAN(J+1)(1:2),          01 09010
     3    J = 1, 3, 2)                                                  01 09020
      ELSE                                                              01 09030
C                                                                       01 09040
C-TOTAL BRANCHING WAS GIVEN.                                            01 09050
C                                                                       01 09060
          WRITE(36, 9010)                                               01 09070
     1    (   AN(J)(1:8), DAN(J)(1:2), AN(J+1), DAN(J+1)(1:2),          01 09080
     3    J = 1, 3, 2)                                                  01 09090
      ENDIF                                                             01 09100
      CALL CNVU2S(FLOGT, DFLT, AN(5), 8, DAN(5), 2)                     01 09110
      WRITE(36, 9011) 'LOG PARTIAL T1/2 =', AN(5), DAN(5)               01 09120
      IF (A .EQ. 0.) THEN                                               01 09130
         WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'               01 09140
         GOTO 900                                                       01 09150
      ENDIF                                                             01 09160
C                                                                       01 09170
C-TEST IF POSITRON OR NEGATRON.                                         01 09180
C-SET NPOS = -1 FOR B- DECAY.                                           01 09190
C                                                                       01 09200
      NPOS = -1                                                         01 09210
      IF (IE .EQ. 1) WRITE(36, 9012) 'FIRST-FORBIDDEN-UNIQUE'           01 09220
      IF (IE .EQ. 2) WRITE(36, 9012) 'SECOND-FORBIDDEN UNIQUE'          01 09230
C                                                                       01 09240
C-IZ < 0 FOR B- DECAY.                                                  01 09250
C-IZ = 0 IS AN ERROR.                                                   01 09260
C-IZ > 0 FOR EC, B+ DECAY.                                              01 09270
C                                                                       01 09280
      IF(IZ.EQ.0) THEN                                                  01 09290
         WRITE(36, 9004) 'Z VALUE MISSING. OLD CARD KEPT'               01 09300
         GOTO 900                                                       01 09310
      ENDIF                                                             01 09320
      IF(IZ.GT.0) THEN                                                  01 09330
C                                                                       01 09340
C-ELECTRON CAPTURE ALLOWED.                                             01 09350
C                                                                       01 09360
         FC = CAPTUR(IZ, W0, IPRINT, 0, FCK, FCL, FCMNO)                01 09370
         FC1 = 0.                                                       01 09380
         IF (IE .EQ. 0) THEN                                            01 09390
             FCP = CAPTUR(IZ, W0 + DW0, IPRINT, 0, FCKP, FCLP, FCMNP)   01 09400
             FCM = CAPTUR(IZ, W0 - DW0, IPRINT, 0, FCKM, FCLM, FCMNM)   01 09410
         ELSE                                                           01 09420
C                                                                       01 09430
C-CAPTURE FOR FIRST OR SECOND FORBIDDEN UNIQUE.                         01 09440
C                                                                       01 09450
             FC1 = CAPTUR(IZ, W0, IPRINT, IE, FCK, FCL, FCMN1)          01 09460
             FCP = CAPTUR(IZ, W0 + DW0, IPRINT, IE, FCKP, FCLP, FCMNP)  01 09470
             FCM = CAPTUR(IZ, W0 - DW0, IPRINT, IE, FCKM, FCLM, FCMNM)  01 09480
             DELF=ALOG10(FC1 / FC)                                      01 09490
             FC = FC1                                                   01 09500
             FCMNO = FCMN1                                              01 09510
             IF (IPRINT .NE. 0) WRITE(36, 9013) ' LOG(F', IE,           01 09520
     1        '/F0) FOR ELECTRON CAPTURE = ', DELF                      01 09530
         ENDIF                                                          01 09540
C                                                                       01 09550
C-TEST IF ENOUGH ENERGY FOR POSITRONS.                                  01 09560
C-SET NPOS = 0 FOR EC-ONLY DECAY.                                       01 09570
C                                                                       01 09580
         NPOS = 0                                                       01 09590
         AREA = 0.                                                      01 09600
         AREAP = 0.                                                     01 09610
         AREAM = 0.                                                     01 09620
         AREA1U = 0.                                                    01 09630
C                                                                       01 09640
         IF (W0 .LE. 3.01) THEN                                         01 09650
            AREA = AREA + FC                                            01 09660
            AREAP = AREAP + FCP                                         01 09670
            AREAM = AREAM + FCM                                         01 09680
            IF (AREA .LE. 0.) THEN                                      01 09690
C                                                                       01 09700
C-ERROR EXIT FOR NEGATIVE OR ZERO INTEGRATION.                          01 09710
C                                                                       01 09720
                WRITE(36, 9000)                                         01 09730
     1            '0   ZERO OR NEGATIVE INTEGRATION. OLD CARD KEPT'     01 09740
                ISKIP1 = 1                                              01 09750
                somskp=.TRUE.                                           01 09760
                GOTO 900                                                01 09770
            ENDIF                                                       01 09780
            GO TO 410                                                   01 09790
         ENDIF                                                          01 09800
C                                                                       01 09810
C-POSITRON DECAY SETUP IF B+ IS POSSIBLE.                               01 09820
C                                                                       01 09830
         W0 = W0 - 2.                                                   01 09840
C                                                                       01 09850
C-SET NPOS = +1 FOR COMBINED EC AND B+ DECAY.                           01 09860
C                                                                       01 09870
         NPOS = 1                                                       01 09880
      ENDIF                                                             01 09890
C                                                                       01 09900
C-INTEGRATION PROCEDURE USED FOR EITHER B+ OR B-.                       01 09910
C                                                                       01 09920
C-SET UP CONSTANTS TO BE USED IN INTEGRATION.                           01 09930
C                                                                       01 09940
      CALL SETUP(A, IZ, W0 + DW0, 0)                                    01 09950
C                                                                       01 09960
C-CALL INTEGRATION SUBROUTINE.                                          01 09970
C                                                                       01 09980
C      CALL SIMCON(ZL, W0+DW0, PREC, NTRY, AREAP, EAREAP, NOI, RSIM, R2)01 09990
C     Convergence problem solved so heurestic solution removed (TWB.    01 10000
C        930212)                                                        01 10010
      NF=1                                                              01 10020
      CALL QROMB(FNEW,ZL,W0+DW0,AREAP)                                  01 10030
      CALL QROMB(XF,ZL,W0+DW0,EAREAP)                                   01 10040
      CALL SETUP(A, IZ, W0 - DW0, 0)                                    01 10050
C      CALL SIMCON(ZL, W0-DW0, PREC, NTRY, AREAM, EAREAM, NOI, RSIM, R2)01 10060
      NF=1                                                              01 10070
      CALL QROMB(FNEW,ZL,W0-DW0,AREAM)                                  01 10080
      CALL QROMB(XF,ZL,W0-DW0,EAREAM)                                   01 10090
      CALL SETUP(A, IZ, W0, 0)                                          01 10100
C      CALL SIMCON(ZL, W0, PREC, NTRY, AREA, EAREA, NOI, RSIM, R2)      01 10110
      NF=1                                                              01 10120
      CALL QROMB(FNEW,ZL,W0,AREA)                                       01 10130
      CALL QROMB(XF,ZL,W0,EAREA)                                        01 10140
C                                                                       01 10150
C-TEST FOR CONVERGENCE OF INTEGRATION. for SIMCON                       01 10160
C                                                                       01 10170
C      IF (RSIM .GT. PREC)                                              01 10180
C     1     WRITE(36, 9014) RSIM, 'DESIRED PRECISION NOT REACHED'       01 10190
C                                                                       01 10200
C-CALCULATE AVERAGE ENERGY.                                             01 10210
C                                                                       01 10220
      IF (IE .NE. 0) THEN                                               01 10230
C                                                                       01 10240
C-INTEGRATE FOR FIRST OR SECOND FORBIDDEN UNIQUE.                       01 10250
C                                                                       01 10260
          CALL SETUP(A, IZ, W0 - DW0, IE)                               01 10270
C          CALL SIMCON(ZL, W0-DW0, PREC,NTRY,AREAM,EAREAM,NOI,RSIM,R2)  01 10280
          NF=1                                                          01 10290
          CALL QROMB(FNEW,ZL,W0-DW0,AREAM)                              01 10300
          CALL QROMB(XF,ZL,W0-DW0,EAREAM)                               01 10310
          CALL SETUP(A, IZ, W0 + DW0, IE)                               01 10320
C          CALL SIMCON(ZL, W0+DW0, PREC,NTRY,AREAP,EAREAP,NOI,RSIM,R2)  01 10330
          NF=1                                                          01 10340
          CALL QROMB(FNEW,ZL,W0+DW0,AREAP)                              01 10350
          CALL QROMB(XF,ZL,W0+DW0,EAREAP)                               01 10360
          CALL SETUP(A, IZ, W0, IE)                                     01 10370
C          CALL SIMCON(ZL, W0, PREC, NTRY, AREA1U, EAR1U, NOU, RSIM, R2N01 10380
          NF=1                                                          01 10390
          CALL QROMB(FNEW,ZL,W0,AREA1U)                                 01 10400
          CALL QROMB(XF,ZL,W0,EAR1U)                                    01 10410
C          IF (RSIM .GT. PREC) WRITE(36, 9015) '  DESIRED ',IE,         01 10420
C     1        'U PRECISION NOT REACHED',RSIM                           01 10430
C                                                                       01 10440
C-SAVE LOG(F1/F0).                                                      01 10450
C                                                                       01 10460
          DELF = ALOG10(AREA1U / AREA)                                  01 10470
          AREA = AREA1U                                                 01 10480
          EAREA = EAR1U                                                 01 10490
          IF (IPRINT .NE. 0) WRITE(36, 9013)  ' LOG(F', IE,             01 10500
     1       '/F0) = ', DELF, ' FOR BETAS, + OR -'                      01 10510
      ENDIF                                                             01 10520
      EBAR = EAREA / AREA                                               01 10530
      EBARP = EAREAP / AREAP                                            01 10540
      EBARM = EBAR                                                      01 10550
      IF (AREAM .NE. 0.) EBARM = EAREAM / AREAM                         01 10560
      DEBAR = ELMASS * 1000. *                                          01 10570
     1    AMAX1(ABS(EBAR - EBARP), ABS(EBAR - EBARM))                   01 10580
      EBAR = (EBAR - 1.) * ELMASS                                       01 10590
      EBARR = EBAR / ELMASS / (W0 - ZL)                                 01 10600
      EBAR = EBAR * 1000.                                               01 10610
C                                                                       01 10620
C-IZ < 0 FOR B- DECAY.                                                  01 10630
C-IZ = 0 IS AN ERROR. (took care before)                                01 10640
C-IZ > 0 FOR EC / B+ DECAY.                                             01 10650
C                                                                       01 10660
      IF(IZ.GT.0) THEN                                                  01 10670
C                                                                       01 10680
C-CALCULATE CAPTURE TO POSITRON RATIO.                                  01 10690
C-RESTORE FROM POSITRON ENERGY TO TOTAL DECAY ENERGY.                   01 10700
C                                                                       01 10710
         W0 = W0 + 2.                                                   01 10720
C        Must protect against floating overflows for either very small  01 10730
C          capture or positron decay (TWB. 921216)                      01 10740
         IF(FC .LE. AREA)THEN                                           01 10750
            ETOP = FC / AREA                                            01 10760
            ETOPP = FCP / AREAP                                         01 10770
            ETOPM = ETOP                                                01 10780
            IF (AREAM .NE. 0.) ETOPM = FCM / AREAM                      01 10790
C                                                                       01 10800
C-DETOP USED FOR SQUARE OF UNCERTAINTY IN EC / POSITRON RATIO.          01 10810
C                                                                       01 10820
            DETOP = (AMAX1(ABS(ETOP - ETOPP), ABS(ETOP - ETOPM))) ** 2  01 10830
C                                                                       01 10840
C-ADD A 1% UNCERTAINTY FOR THE THEORY.                                  01 10850
C                                                                       01 10860
            DETOP = DETOP + 0.0001 * ETOP * ETOP                        01 10870
            DETOP = SQRT(DETOP)                                         01 10880
         ELSE                                                           01 10890
C           Calculate positron to capture ratio instead                 01 10900
            ETOP=AREA/FC                                                01 10910
            ETOPP=AREAP/FCP                                             01 10920
            ETOPM=ETOP                                                  01 10930
            IF(FCM .NE. 0)ETOPM=AREAM/FCM                               01 10940
            DETOP = (AMAX1(ABS(ETOP - ETOPP), ABS(ETOP - ETOPM))) ** 2  01 10950
            DETOP = DETOP + 0.0001 * ETOP * ETOP                        01 10960
            DETOP = SQRT(DETOP)                                         01 10970
C           Now invert to get back to capture to positron ratio         01 10980
            DETOP=(DETOP/ETOP)/ETOP                                     01 10990
            ETOP=1.0/ETOP                                               01 11000
         ENDIF                                                          01 11010
         EKTOP = FCK / AREA                                             01 11020
         ETOPL = ALOG10(ETOP)                                           01 11030
C                                                                       01 11040
         IF  (IBONLY .EQ. 1) THEN                                       01 11050
C                                                                       01 11060
C-POSITRON INTENSITY ALONE WAS GIVEN.                                   01 11070
C                                                                       01 11080
            PINT = PERC                                                 01 11090
            EINT = PERC * ETOP                                          01 11100
            DPINT = DPERC                                               01 11110
            DEINT = EINT *                                              01 11120
     1      (DPERC * DPERC / PERC / PERC + DETOP * DETOP / ETOP / ETOP) 01 11130
            PERC = PINT + EINT                                          01 11140
            DPERC = PERC * SQRT(DPINT * DPINT / PINT / PINT +           01 11150
     1       DETOP * DETOP / (1. + ETOP) ** 2)                          01 11160
            TP = 100. * THALF(INUMP) / PERC                             01 11170
            DTP = TP *                                                  01 11180
     1       SQRT(DT(INUMP) * DT(INUMP) / THALF(INUMP) / THALF(INUMP)   01 11190
     1         + DPERC * DPERC / PERC / PERC)                           01 11200
            DFLT = DTP * 0.43429 / TP                                   01 11210
            FLOGT = ALOG10(TP)                                          01 11220
            WRITE(36, 9016)                                             01 11230
     1       'LOG OF POSITRON PLUS CAPTURE PARTIAL HALF-LIFE = ',FLOGT  01 11240
            CALL CNVU2S(PERC, DPERC, AN(3), 8, DAN(3), 2)               01 11250
            CALL CNVU2S(TP, DTP, AN(4), 12, DAN(4), 2)                  01 11260
            WRITE(36, 9010)                                             01 11270
     1       (   AN(J)(1:8), DAN(J)(1:2),                               01 11280
     2        AN(J+1),DAN(J+1)(1:2),J = 1, 3, 2)                        01 11290
         ELSE                                                           01 11300
C                                                                       01 11310
C-TOTAL INTENSITY (EC AND B+) WAS GIVEN.                                01 11320
C                                                                       01 11330
C           Cannot use ETOP+-DETOP to calculate positron intensity when 01 11340
C             electron to capture ratio is large --- DPINT possibly     01 11350
C             overestimated. Cannot use PERC-PINT when electron to      01 11360
C*            capture ratio is low --- possible machine roundoff error  01 11370
            IF(FC .LE. AREA)THEN                                        01 11380
               EINT=PERC*(ETOP/(1+ETOP))                                01 11390
               PINT = PERC / (1. + ETOP)                                01 11400
               DPINT = PINT * SQRT(DPERC * DPERC / PERC / PERC +        01 11410
     1           DETOP * DETOP / (1. + ETOP) ** 2)                      01 11420
            ELSE                                                        01 11430
               PINT=AREA/(AREA+FC)                                      01 11440
               PINTP=AREAP/(AREAP+FCP)                                  01 11450
               PINTM=AREAM/(AREAM+FCM)                                  01 11460
               DPINT=AMAX1(ABS(PINTP-PINT),ABS(PINT-PINTM))**2          01 11470
C              Account for uncertainty in theory                        01 11480
               DPINT=DPINT+(0.01*ETOP/(1.+ETOP)**2)**2                  01 11490
               DPINT=SQRT(PERC*PERC*DPINT + (PINT*DPERC)**2)            01 11500
               PINT=PERC*PINT                                           01 11510
               EINT = PERC - PINT                                       01 11520
               DEINT=(DETOP/ETOP)**2                                    01 11530
               DEINT=DEINT/((1.+ETOP)**2)                               01 11540
               DEINT=EINT*SQRT(DPERC*DPERC/PERC/PERC +DEINT)            01 11550
            ENDIF                                                       01 11560
         ENDIF                                                          01 11570
C   changes the order of this formula due to overflow                   01 11580
C             DEINT = EINT * SQRT(DPERC * DPERC / PERC / PERC +         01 11590
C     1        DETOP * DETOP / (ETOP * (1. + ETOP)) ** 2)               01 11600
C                                                                       01 11610
               DEINT=(DETOP/ETOP)**2                                    01 11620
               DEINT=DEINT/((1.+ETOP)**2)                               01 11630
               DEINT=EINT*SQRT(DPERC*DPERC/PERC/PERC +DEINT)            01 11640
C                                                                       01 11650
C        NOTE: pint and eint are now correlated                         01 11660
C                                                                       01 11670
C                                                                       01 11680
C-PRINT CAPTURE AND POSITRON RATIOS AND INTENSITIES.                    01 11690
C                                                                       01 11700
         WRITE(36, 9017) ETOP, DETOP, ETOPL, EKTOP                      01 11710
         WRITE(36, 9018) PINT, DPINT, EINT, DEINT                       01 11720
C                                                                       01 11730
C-COMBINE POSITRON AND CAPTURE.                                         01 11740
C-ALSO ENTER HERE WITH NPOS = 0 IF EC ONLY (E < 1022).                  01 11750
C                                                                       01 11760
         AREA = AREA + FC                                               01 11770
         AREAP = AREAP + FCP                                            01 11780
         AREAM = AREAM + FCM                                            01 11790
         IF (AREA .LE. 0.) THEN                                         01 11800
C                                                                       01 11810
C-ERROR EXIT FOR NEGATIVE OR ZERO INTEGRATION.                          01 11820
C                                                                       01 11830
             WRITE(36, 9000)                                            01 11840
     1          '0   ZERO OR NEGATIVE INTEGRATION. OLD CARD KEPT'       01 11850
             ISKIP1 = 1                                                 01 11860
             somskp=.TRUE.                                              01 11870
             GOTO 900                                                   01 11880
         ENDIF                                                          01 11890
      ENDIF                                                             01 11900
410   CONTINUE                                                          01 11910
C     Moved call to WECARD to here so program has access to the new     01 11920
C       E record contents (TWB. 930212)                                 01 11930
      IF (IZ .GE. 0) THEN                                               01 11940
C                                                                       01 11950
C-SKIP FOLLOWING SECTION FOR B- DECAY.                                  01 11960
C                                                                       01 11970
C-PLACE IE, IB IN PROPER FIELDS ON NEW FIRST E-CARD.                    01 11980
C                                                                       01 11990
         CALL WECARD                                                    01 12000
ctwb         IF (EK(INUMP) .LE. 1022.) GOTO 530                         01 12010
      ENDIF                                                             01 12020
C                                                                       01 12030
C-COMBINE LOG F AND LOG T FOR EITHER B- OR EC / B+.                     01 12040
C                                                                       01 12050
      FLOG = ALOG10(AREA)                                               01 12060
      FLOGP = ALOG10(AREAP)                                             01 12070
      FLOGM = FLOG                                                      01 12080
      IF (AREAM .GT. 0.) FLOGM = ALOG10(AREAM)                          01 12090
      FLOGF = FLOGT + FLOG                                              01 12100
      FT = AREA * TP                                                    01 12110
      DLAREA = AMAX1(ABS(FLOG - FLOGP), ABS(FLOG - FLOGM))              01 12120
      DLFT = SQRT(DLAREA * DLAREA + DFLT * DFLT)                        01 12130
C                                                                       01 12140
      FTOUT = FLOGF                                                     01 12150
      IF (AREAM .LE. 0.) FTOUT = FLOGT + FLOGP                          01 12160
C     Program was not always outputing proper significant digits        01 12170
C       (TWB. 930212)                                                   01 12180
      IF (ZERUNC .NE. 0                                                 01 12190
     2  .AND. ((CARD(30:30) .GE. 'A' .AND. CARD(30:30) .LE. 'Z')        01 12200
     3  .OR. (CARD(40:40) .GE. 'A' .AND. CARD(40:40) .LE. 'Z')          01 12210
     4  .OR. (CARD(30:31) .EQ. '  ' .AND. CARD(40:41) .EQ. '  '))) THEN 01 12220
C         PRINT ONLY TO FIRST DECIMAL PLACE                             01 12230
          CALL CNVU2S(FTOUT, 0.3, AN(1), 8, DAN(1), 2)                  01 12240
          DAN(1) = ' '                                                  01 12250
          IF(UNCERT.NE.' ') CALL UNCALP(DAN(1),UNCERT,1)                01 12260
      ELSE                                                              01 12270
          CALL CNVU2S(FTOUT, DLFT, AN(1), 8, DAN(1), 2)                 01 12280
      ENDIF                                                             01 12290
C                                                                       01 12300
C    blank out if number is all zero                                    01 12310
C                                                                       01 12320
      CALL ZBLANK(AN(1)(1:8),DAN(1))                                    01 12330
      CARD(42:49)=AN(1)(1:8)                                            01 12340
      CALL CENTER(CARD(42:49),8)                                        01 12350
      CARD(50:55)=' '                                                   01 12360
      CARD(50:51)=DAN(1)(1:2)                                           01 12370
      IF (AREAM .LE. 0.) CARD(50:51)='LE'                               01 12380
C                                                                       01 12390
C-RE-ENTER HERE FOR B- AS WELL AS FOR EC/B+.                            01 12400
C                                                                       01 12410
C     Changed from check on pint to check if IB was output (TWB. 930212)01 12420
      IF (C8 .EQ. 'E' .AND. card(22:29) .EQ. ' ')GOTO 530               01 12430
C                                                                       01 12440
C-PUT AVERAGE B- OR B+ ENERGY ON SECOND E-CARD.                         01 12450
C                                                                       01 12460
C     Program was assuming fractional uncertainty but using absolute    01 12470
C       (TWB. 930212)                                                   01 12480
      DEE = DEBAR/ebar                                                  01 12490
C                                                                       01 12500
C-DON'T SHOW UNCERTAINTY IF LESS THAN 0.01%.                            01 12510
C-ROUND OFF AS IF UNCERTAINTY IS 0.1 KEV.                               01 12520
C     Changed from 0.1% and rewrote algorithm (TWB. 930212)             01 12530
C                                                                       01 12540
      If(dee .le. 0.0001)Then                                           01 12550
         Call cnvu2s(ebar,0.1,an(1),11,stb,1)                           01 12560
      Else                                                              01 12570
         CALL CNVU2S(EBAR, DEBAR, AN(1), 11,STB,-2)                     01 12580
      Endif                                                             01 12590
      IF (AN(1)(1:8) .NE. STARS) THEN                                   01 12600
         Call Addstr(card2,10,'EAV=')                                   01 12610
         Call Lbsup(an(1)(1:11))                                        01 12620
         Call Addstr(card2,14,an(1)(1:Lenstr(an(1))))                   01 12630
         If(card2(14+Lenstr(an(1)):) .NE. ' ')                          01 12640
     2     Call Addstr(card2,14+Lenstr(an(1)),'$')                      01 12650
      ENDIF                                                             01 12660
  530 WRITE(36, 9020) EK(INUMP), IE, FLOG, DLAREA                       01 12670
      WRITE(36, 9021) IE, FLOGF, DLFT, IE, FT                           01 12680
      IF (NPOS .NE. 0) WRITE(36, 9022) EBAR, DEBAR, EBARR               01 12690
C                                                                       01 12700
C   Unless dit is the last parent card value, loop back to 300          01 12710
C                                                                       01 12720
      IF(INUMP.LT.NUMP) GO TO 300                                       01 12730
C                                                                       01 12740
      WRITE(36, 9023) CARDI, 'OLD CARD'                                 01 12750
      IF (ISKIP + ISKIP1 .NE. 0) THEN                                   01 12760
C                                                                       01 12770
C-IF THERE WAS AN ERROR, THEN PRINT AND PUNCH OLD CARD.                 01 12780
C                                                                       01 12790
          WRITE(36, 9024) 'OLD CARD KEPT'                               01 12800
          WRITE(21, 9000) CARDI                                         01 12810
         crdcom='*****'                                                 01 12820
          GOTO 100                                                      01 12830
      ENDIF                                                             01 12840
C                                                                       01 12850
C-IF NO ERRORS, THEN PRINT AND PUNCH TWO NEW CARDS.                     01 12860
C                                                                       01 12870
      WRITE(36, 9025) CARD, 'NEW CARD'                                  01 12880
      WRITE(21, 9000) CARD                                              01 12890
      CARD2(1:5)=CARD(1:5)                                              01 12900
      CARD2(6:6)='S'                                                    01 12910
      CARD2(8:8)=C8                                                     01 12920
      WRITE(36, 9025) CARD2, 'NEW CARD'                                 01 12930
      WRITE(21, 9000) CARD2                                             01 12940
      If(dowarn)Then                                                    01 12950
         Write(6,FMT='(1X,A)')card                                      01 12960
         Write(6,FMT='(A)')                                             01 12970
     2     '  ***** Allowed spectrum assumed for calcuations *****'     01 12980
         Write(36,FMT='(A)')                                            01 12990
     2     '  ***** Allowed spectrum assumed for calcuations *****'     01 13000
         crdcom=card(1:5)                                               01 13010
         crdcom(7:7)='c'                                                01 13020
         crdcom(8:8)=c8                                                 01 13030
         crdcom(10:)='LOGFT'                                            01 13040
         If(c8 .EQ. 'E')Then                                            01 13050
            If(card(22:29) .NE. ' ')                                    01 13060
     2        Call Addstr(crdcom,Lenstr(crdcom)+1,',IB')                01 13070
            If(card(32:39) .NE. ' ')                                    01 13080
     2        Call Addstr(crdcom,Lenstr(crdcom)+1,',IE')                01 13090
         EndIf                                                          01 13100
         Call Addstr(crdcom,Lenstr(crdcom)+1,'$')                       01 13110
         crdcom(Lenstr(crdcom)+1:)=                                     01 13120
     2     'Allowed spectrum assumed for calcuations by ^LOGFT'         01 13130
         Write(21,9000)crdcom                                           01 13140
      EndIf                                                             01 13150
      GO TO 100                                                         01 13160
C                                                                       01 13170
C-CHECK OLD SECOND CARDS.(B OR E)                                       01 13180
C           ============                                                01 13190
C                                                                       01 13200
  600 IF(C6.EQ.'S' .OR. C6.EQ.'2') THEN                                 01 13210
         IF(C79.EQ.' B' .OR. C79.EQ.' E') THEN                          01 13220
            WRITE(36, 9002) CARD                                        01 13230
C           Change column 6 to "S" after writing the card instead of    01 13240
C             before (TWB. 930212)                                      01 13250
            CARD(6:6)='S'                                               01 13260
            IF (A .EQ. 0.) THEN                                         01 13270
               WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'         01 13280
               crdcom='*****'                                           01 13290
               GOTO 900                                                 01 13300
            ENDIF                                                       01 13310
            IF (ISKIP + ISKIP1 .EQ. 0) THEN                             01 13320
C                                                                       01 13330
C-IF A SECOND CARD WAS THERE, THEN PRINT IT ALSO FOR REFERENCE.         01 13340
C                                                                       01 13350
               WRITE(36, 9004) 'CHECK OLD SECOND CARD'                  01 13360
               WRITE(36, 9000) '0'                                      01 13370
               GOTO 100                                                 01 13380
            ENDIF                                                       01 13390
         ENDIF                                                          01 13400
      Else                                                              01 13410
C        Had to allow for evaluators using other than "2" or "S"        01 13420
C          (TWB. 930212)                                                01 13430
         If(c6 .NE. ' ' .AND. c79 .EQ. ' B')Then                        01 13440
            If(Indexf(card,10,'EAV=') .GT. 0)Then                       01 13450
               WRITE(36, 9002) CARD                                     01 13460
               CARD(6:6)='S'                                            01 13470
               IF (A .EQ. 0.) THEN                                      01 13480
                  WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'      01 13490
                  crdcom='*****'                                        01 13500
                  GOTO 900                                              01 13510
               ENDIF                                                    01 13520
               IF (ISKIP + ISKIP1 .EQ. 0) THEN                          01 13530
                  WRITE(36, 9004) 'CHECK OLD SECOND CARD'               01 13540
                  WRITE(36, 9000) '0'                                   01 13550
                  GOTO 100                                              01 13560
               ENDIF                                                    01 13570
            Endif                                                       01 13580
         Endif                                                          01 13590
         If(c6 .NE. ' ' .AND. c79 .EQ. ' E')Then                        01 13600
            If(Indexf(card,10,'EAV=') .GT. 0                            01 13610
     2         .OR. Indexf(card,10,'CK=') .GT. 0                        01 13620
     3         .OR. Indexf(card,10,'CL=') .GT. 0                        01 13630
     4         .OR. Indexf(card,10,'CM+=') .GT. 0)Then                  01 13640
               WRITE(36, 9002) CARD                                     01 13650
               CARD(6:6)='S'                                            01 13660
               IF (A .EQ. 0.) THEN                                      01 13670
                  WRITE(36, 9004) 'A VALUE MISSING. OLD CARD KEPT'      01 13680
                  crdcom='*****'                                        01 13690
                  GOTO 900                                              01 13700
               ENDIF                                                    01 13710
               IF (ISKIP + ISKIP1 .EQ. 0) THEN                          01 13720
                  WRITE(36, 9004) 'CHECK OLD SECOND CARD'               01 13730
                  WRITE(36, 9000) '0'                                   01 13740
                  GOTO 100                                              01 13750
               ENDIF                                                    01 13760
            Endif                                                       01 13770
         Endif                                                          01 13780
      ENDIF                                                             01 13790
C                                                                       01 13800
C-PRODUCE NEW DATA SET.                                                 01 13810
C                                                                       01 13820
  900 If(dowarn .AND. crdcom.NE.'*****')Then                            01 13830
         If(card .NE. crdcom)Write(21,9000)card                         01 13840
      Else                                                              01 13850
         WRITE(21, 9000) CARD                                           01 13860
      EndIf                                                             01 13870
      GOTO 100                                                          01 13880
C                                                                       01 13890
C-HERE ON END OF INPUT.                                                 01 13900
C                                                                       01 13910
 1265 Continue                                                          01 13920
      If(iskip .gt. 0)Then                                              01 13930
         Write(6,9002)'***** Data set will not be modified - '//        01 13940
     2     'See LOGFT report'                                           01 13950
      Else                                                              01 13960
         If(somskp)Write(6,9002)'***** Some records will not be '//     01 13970
     2     'modified - See LOGFT report'                                01 13980
      Endif                                                             01 13990
      WRITE(36, 9000) '0  ALL PROBLEMS COMPLETED'                       01 14000
C+++MDC+++                                                              01 14010
C...VAX                                                                 01 14020
C/      CALL EXIT                                                       01 14030
C...DVF,UNX,ANS                                                         01 14040
      STOP                                                              01 14050
C---MDC---                                                              01 14060
C                                                                       01 14070
      END                                                               01 14080
        REAL FUNCTION XF(X)                                             02 00010
C                                                                       02 00020
C  Used to calculate the average B+- energies                           02 00030
C                                                                       02 00040
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************02 00050
C*                           Present Code                               02 00060
C*                           ------------                               02 00070
C*  FNEW                                                                02 00080
C*                                                                      02 00090
C***********************************************************************02 00100
        REAL X                                                          02 00110
C                                                                       02 00120
        REAL FNEW                                                       02 00130
        EXTERNAL FNEW                                                   02 00140
C                                                                       02 00150
        REAL Y                                                          02 00160
C                                                                       02 00170
        Y=FNEW(X)                                                       02 00180
        IF(X .NE. 0. .AND. Y .NE. 0.)THEN                               02 00190
           XF=X*Y                                                       02 00200
        ELSE                                                            02 00210
           XF=0.                                                        02 00220
        ENDIF                                                           02 00230
        RETURN                                                          02 00240
        END                                                             02 00250
C-----  Integration Routines                                            03 00010
        SUBROUTINE QROMB(FUNC,A,B,SS)                                   03 00020
C  Returns as SS the integral of the function FUNC FROM A TO B.         03 00030
C    Integration is performed by Romberg's method of order 2K,          03 00040
C    where, e.g., K=2 is Simpson's Rule                                 03 00050
C    [Numerical Recipes (The Art of Scientific Computing).  W.H. Press, 03 00060
C    et al.  Cambridge University Press (NY, 1986), p.114]              03 00070
C                                                                       03 00080
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************03 00090
C*                           Present Code                               03 00100
C*                           ------------                               03 00110
C*  POLINT  TRAPZD                                                      03 00120
C*                                                                      03 00130
C*                        FORTRAN 77 Supplied                           03 00140
C*                        -------------------                           03 00150
C*  ABS                                                                 03 00160
C*                                                                      03 00170
C***********************************************************************03 00180
        REAL FUNC,A,B,SS                                                03 00190
        EXTERNAL FUNC                                                   03 00200
C                                                                       03 00210
        INTEGER JMAX,JMAXP,K,KM                                         03 00220
C        PARAMETER (JMAX=30,JMAXP=JMAX+1,K=3,KM=K-1)                    03 00230
        PARAMETER (JMAX=20,JMAXP=JMAX+1,K=3,KM=K-1)                     03 00240
C                                                                       03 00250
        REAL EPS,DUM                                                    03 00260
        COMMON/PREC1/EPS,DUM                                            03 00270
C                                                                       03 00280
        REAL S(JMAXP),H(JMAXP),DSS                                      03 00290
        INTEGER J,JJ                                                    03 00300
C                                                                       03 00310
        REAL ABS,ALOG10                                                 03 00320
        INTRINSIC ABS,ALOG10,DABS,DBLE,INT                              03 00330
        Save                                                            03 00340
C                                                                       03 00350
      SS=0.                                                             03 00360
      DSS=0.                                                            03 00370
        H(1)=1.                                                         03 00380
        DO 100 J=1,JMAX                                                 03 00390
           JJ=J                                                         03 00400
           CALL TRAPZD(FUNC,A,B,S(J),JJ)                                03 00410
                                                                        03 00420
           IF(J .GE. K)THEN                                             03 00430
              CALL POLINT(H(J-KM),S(J-KM),K,0.,SS,DSS)                  03 00440
C       Add check for when integral is zero                             03 00450
              IF(SS .EQ. 0. .AND. DSS .EQ. 0.)RETURN                    03 00460
C              IF(ABS(DSS) .LE. EPS*ABS(SS))RETURN                      03 00470
              IF(DABS(DBLE(DSS)/DBLE(SS)) .LE. EPS)RETURN               03 00480
           ENDIF                                                        03 00490
           S(J+1)=S(J)                                                  03 00500
           H(J+1)=0.25*H(J)                                             03 00510
100     CONTINUE                                                        03 00520
        PAUSE 'TOO MANY STEPS --- QROMB'                                03 00530
        END                                                             03 00540
C                                                                       04 00010
        SUBROUTINE TRAPZD(FUNC,A,B,S,N)                                 04 00020
C  Compute's the Nth stage of refinement of an extended trapoziodal rule04 00030
C    FUNC is input as the name of the function to be integrated between 04 00040
C    limits A and B, also input.  S should not be modified between      04 00050
C    sequential calls.                                                  04 00060
C    [Numerical Recipes (The Art of Scientific Computing).  W.H. Press, 04 00070
C    et al.  Cambridge University Press (NY, 1986), p.111]              04 00080
C                                                                       04 00090
C  Changed to double precision where necessary to avoid convergence     04 00100
C    problems (TWB. 930212)                                             04 00110
C                                                                       04 00120
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************04 00130
C*                           Present Code                               04 00140
C*                           ------------                               04 00150
C*  FUNC*                                                               04 00160
C*                                                                      04 00170
C*  *Dummy routine                                                      04 00180
C*                                                                      04 00190
C***********************************************************************04 00200
        INTEGER N                                                       04 00210
        REAL FUNC,A,B,S                                                 04 00220
        EXTERNAL FUNC                                                   04 00230
C                                                                       04 00240
        INTEGER IT,J                                                    04 00250
      DOUBLE PRECISION TNM,DEL,SUM,X                                    04 00260
C                                                                       04 00270
      REAL REAL                                                         04 00280
      DOUBLE PRECISION DBLE                                             04 00290
      INTRINSIC DBLE,REAL                                               04 00300
      Save                                                              04 00310
C                                                                       04 00320
        IF(N .EQ. 1)THEN                                                04 00330
           S=0.5*(B-A)*(FUNC(A)+FUNC(B))                                04 00340
           IT=1                                                         04 00350
        ELSE                                                            04 00360
           TNM=DBLE(IT)                                                 04 00370
           DEL=(DBLE(B)-DBLE(A))/TNM                                    04 00380
           X=A+0.5D+0*DEL                                               04 00390
           SUM=0.D+0                                                    04 00400
           DO 100 J=1,IT                                                04 00410
              SUM=SUM+DBLE(FUNC(REAL(X)))                               04 00420
              X=X+DEL                                                   04 00430
100        CONTINUE                                                     04 00440
           S=0.5D+0*(S+(DBLE(B)-DBLE(A))*SUM/TNM)                       04 00450
           IT=2*IT                                                      04 00460
        ENDIF                                                           04 00470
        RETURN                                                          04 00480
        END                                                             04 00490
        SUBROUTINE POLINT(XA,YA,N,X,Y,DY)                               05 00010
C  Given arrays XA and YA, each of length N, and given a value X, THE   05 00020
C    value Y and an error estimate DY are returned                      05 00030
C    [Numerical Recipes (The Art of Scientific Computing.  W.H. Press,  05 00040
C    et al.  Cambridge University Press (NY, 1986), p.82]               05 00050
C                                                                       05 00060
        INTEGER N                                                       05 00070
        REAL XA(N),YA(N),X,Y,DY                                         05 00080
C                                                                       05 00090
        INTEGER NMAX                                                    05 00100
        PARAMETER (NMAX=10)                                             05 00110
C                                                                       05 00120
        INTEGER NS,I,M                                                  05 00130
        REAL C(NMAX),D(NMAX),DIF,DIFT,HO,HP,DEN,W                       05 00140
C                                                                       05 00150
      REAL SCALE,MAXVAL,MINVAL                                          05 00160
      INTEGER TEST                                                      05 00170
C                                                                       05 00180
      REAL ABS,ALOG10,AMAX1,AMIN1                                       05 00190
      INTEGER INT                                                       05 00200
      INTRINSIC ABS,ALOG10,AMAX1,AMIN1,INT                              05 00210
      Save                                                              05 00220
                                                                        05 00230
C                                                                       05 00240
        IF(N .LE. 1 .OR. N .GT. NMAX)THEN                               05 00250
           DY=10000.                                                    05 00260
           RETURN                                                       05 00270
        ENDIF                                                           05 00280
        NS=1                                                            05 00290
        DIF=ABS(X-XA(1))                                                05 00300
      MAXVAL=0.                                                         05 00310
      MINVAL=0.                                                         05 00320
      SCALE=1.                                                          05 00330
        DO 100 I=1,N                                                    05 00340
           DIFT=ABS(X-XA(I))                                            05 00350
           IF(DIFT .LT. DIF)THEN                                        05 00360
              NS=I                                                      05 00370
              DIF=DIFT                                                  05 00380
           ENDIF                                                        05 00390
           C(I)=YA(I)                                                   05 00400
           D(I)=YA(I)                                                   05 00410
           IF(YA(I) .NE. 0.)THEN                                        05 00420
              MAXVAL=AMAX1(MAXVAL,ALOG10(ABS(YA(I))))                   05 00430
              MINVAL=AMIN1(MINVAL,ALOG10(ABS(YA(I))))                   05 00440
           ENDIF                                                        05 00450
100     CONTINUE                                                        05 00460
        Y=YA(NS)                                                        05 00470
C     Try to keep the scale within reasonable limits                    05 00480
      TEST=INT(MINVAL)                                                  05 00490
      IF(TEST .LT. -5)SCALE=10.**(-TEST)                                05 00500
      TEST=INT(MAXVAL)                                                  05 00510
      IF(TEST .GT. 5)SCALE=SCALE*10.**(-TEST)                           05 00520
      IF(SCALE .NE. 1.)THEN                                             05 00530
         Y=Y*SCALE                                                      05 00540
         DO 120 I=1,N                                                   05 00550
            C(I)=SCALE*C(I)                                             05 00560
            D(I)=SCALE*D(I)                                             05 00570
120      CONTINUE                                                       05 00580
      ENDIF                                                             05 00590
        NS=NS-1                                                         05 00600
        DO 200 M=1,N-1                                                  05 00610
           DO 150 I=1,N-M                                               05 00620
              HO=XA(I)-X                                                05 00630
              HP=XA(I+M)-X                                              05 00640
              W=C(I+1)-D(I)                                             05 00650
              DEN=HO-HP                                                 05 00660
              IF(DEN .EQ. 0.)THEN                                       05 00670
                 DY=1000.*Y                                             05 00680
                 IF(SCALE .NE. 1.)THEN                                  05 00690
                    Y=Y/SCALE                                           05 00700
                    DY=DY/SCALE                                         05 00710
                 ENDIF                                                  05 00720
                 RETURN                                                 05 00730
              ENDIF                                                     05 00740
              DEN=W/DEN                                                 05 00750
              D(I)=HP*DEN                                               05 00760
              C(I)=HO*DEN                                               05 00770
150        CONTINUE                                                     05 00780
           IF(2*NS .LT. N-M)THEN                                        05 00790
              DY=C(NS+1)                                                05 00800
           ELSE                                                         05 00810
              DY=D(NS)                                                  05 00820
              NS=NS-1                                                   05 00830
           ENDIF                                                        05 00840
           Y=Y+DY                                                       05 00850
200     CONTINUE                                                        05 00860
      IF(SCALE .NE. 1)THEN                                              05 00870
         Y=Y/SCALE                                                      05 00880
         DY=DY/SCALE                                                    05 00890
      ENDIF                                                             05 00900
        RETURN                                                          05 00910
        END                                                             05 00920
      SUBROUTINE SETUP(A, IZ, W0, IE)                                   06 00010
C                                                                       06 00020
C  ASSOCIATED WITH FUNCTION F TO REDUCE THE NUMBER OF OPERATIONS IN F.  06 00030
C  SETUP MUST BE CALLED BEFORE F.                                       06 00040
C                                                                       06 00050
      REAL A, W0                                                        06 00060
      INTEGER IZ, IE                                                    06 00070
C                                                                       06 00080
      REAL APOW, BPOW, CALPHA, CM50A, CM50B,                            06 00090
     1    CM80A, CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, 06 00100
     2    G1, G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2, TWOG3, TWOR, V, 06 00110
     3    W0X                                                           06 00120
      COMPLEX BQ, BQU                                                   06 00130
      INTEGER IEX, IFIN, IZX                                            06 00140
      COMMON /FCOM/ APOW, BPOW, BQ, BQU, CALPHA, CM50A, CM50B, CM80A,   06 00150
     1    CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, G1,    06 00160
     2    G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2,                     06 00170
     3    TWOG3, TWOR, V, W0X, IEX, IFIN, IZX                           06 00180
C                                                                       06 00190
      REAL ALPHA, ATEMP, C, CALPHS, CM50, CM80,                         06 00200
     1    CUBRTA, GI, GR, GRSQ, PI, RSQ, RSQSQ, TAZR, TGM1,             06 00210
     2    TWOPI, ZR, ZR2, ZR3                                           06 00220
      INTEGER IZTEMP                                                    06 00230
C                                                                       06 00240
      DATA ALPHA/7.2973E-3/                                             06 00250
      DATA PI, TWOPI/3.14 159 265 359, 6.28 318 530 718/                06 00260
      DATA ATEMP, IZTEMP/0., 0/                                         06 00270
      Save                                                              06 00280
C                                                                       06 00290
      IZX = IZ                                                          06 00300
      W0X = W0                                                          06 00310
      IEX = IE                                                          06 00320
C                                                                       06 00330
      IF ((A .EQ. ATEMP) .AND. (IZ .EQ. IZTEMP)) GOTO 200               06 00340
C-CALCULATE NUCLEAR RADIUS (UNLESS ALREADY DONE).                       06 00350
      IF (A .EQ. ATEMP) GOTO 190                                        06 00360
          ATEMP = A                                                     06 00370
          CUBRTA = ATEMP ** 0.33 333 333 333                            06 00380
          R = 2.908E-3 * CUBRTA - 2.437E-3 / CUBRTA                     06 00390
          RSQ = R * R                                                   06 00400
          TWOR = R + R                                                  06 00410
  190 IF (IZ .EQ. IZTEMP) GOTO 240                                      06 00420
          IZTEMP = IZ                                                   06 00430
          C = IZTEMP                                                    06 00440
          C = ABS(C)                                                    06 00450
C-CALCULATE TERMS FOR SCREENING (UNLESS ALREADY DONE).                  06 00460
          V = ((-9.45E-9 * C + 3.014E-6) * C + 1.881E-4) * C - 5.166E-4 06 00470
          IF (IZTEMP .LT. 0) GOTO 230                                   06 00480
              APOW = ((1.11E-7 * C - 1.01E-5) * C - 2.38E-3) * C + 0.10206 00490
              BPOW = ((-2.42E-8 * C + 3.83E-6) * C + 3.60E-5) * C-0.015606 00500
C-FIRST WE COMPUTE THE PART OF THE INTEGRAND THAT IS CONSTANT           06 00510
C-AND CAN BE PUT IN FRONT OF THE INTEGRAL SIGN.                         06 00520
C-1/2 R**2 GAMMASQ(2G + 1).                                             06 00530
  230     CALPHA = C * ALPHA                                            06 00540
          CALPHS = CALPHA * CALPHA                                      06 00550
          G1 = SQRT(1. - CALPHS)                                        06 00560
          TWOG = G1 + G1                                                06 00570
          ZR = TWOG + 1.                                                06 00580
          G1P1 = G1 + 1.                                                06 00590
          TGM1 = TWOG - 1.                                              06 00600
          CALL GAMMA(ZR, 0., GR, GI)                                    06 00610
          GRSQ = GR * GR                                                06 00620
          BQ = CMPLX(ZR, 0.)                                            06 00630
  240 FRONT = 0.5 / RSQ / GRSQ                                          06 00640
      IFIN = 0                                                          06 00650
      IF (IZTEMP .GT. 0) GOTO 75                                        06 00660
C-FOR NEGATRONS, SET UP CONSTANT FOR LOW-E APPROXIMATION.               06 00670
          TAZR = TWOR * CALPHA                                          06 00680
          FRLOW = TWOPI * TWOR * (TAZR ** TGM1) * G1P1 *                06 00690
     1        (1. - TAZR * 3. / ZR)                                     06 00700
C-FOR LARGE Z NEGATRONS, SET UP SCREENING CONSTANTS.                    06 00710
          IF (IZTEMP .GE. -50) GOTO 75                                  06 00720
              CM50 = C - 50.                                            06 00730
              CM50A = -2.5E-3 * CM50 + 1.                               06 00740
              CM50B = -4.0E-6 * CM50 * CM50                             06 00750
              IFIN = -1                                                 06 00760
              GOTO 200                                                  06 00770
C-LARGE Z POSITRON SCREENING CONSTANTS.                                 06 00780
   75 IF (IZTEMP .LE. 80) GOTO 200                                      06 00790
          CM80 = C - 80.                                                06 00800
          CM80A = -1.7E-4 * CM80                                        06 00810
          CM80B = +6.3E-4 * CM80                                        06 00820
          CM80C = -8.8E-3 * CM80                                        06 00830
          IFIN = 1                                                      06 00840
  200 IF (IE .LT. 1) GOTO 99                                            06 00850
C-SET UP FOR FIRST FORBIDDEN UNIQUE.                                    06 00860
      G2 = SQRT(4. - CALPHS)                                            06 00870
      TWOG2 = G2 + G2                                                   06 00880
      ZR2 = TWOG2 + 1.                                                  06 00890
      G2P1 = G2 + 1.                                                    06 00900
      CALL GAMMA(ZR2, 0., GR, GI)                                       06 00910
      BQU = CMPLX(ZR2, 0.)                                              06 00920
      RSQSQ = RSQ * RSQ                                                 06 00930
      CONU = 4.5 / RSQSQ / GR / GR                                      06 00940
      IF (IZTEMP .GT. 0) GOTO 250                                       06 00950
C-SET UP CONSTANT FOR LOW-E NEGATRON FIRST FORBIDDEN DECAY.             06 00960
          CONLU = 72. * PI * CALPHA * (1. + G2P1) *                     06 00970
     1        (TAZR ** (ZR2 - 3.)) * (1. - 2.5 * TAZR / ZR2) /          06 00980
     2        RSQ / GR / GR                                             06 00990
  250 IF (IE .LE. 1) GOTO 99                                            06 01000
C-SET UP FOR SECOND FORBIDDEN UNIQUE.                                   06 01010
      G3 = SQRT(9. - CALPHS)                                            06 01020
      TWOG3 = G3 + G3                                                   06 01030
      ZR3 = TWOG3 + 1.                                                  06 01040
      G3P1 = G3 + 1.                                                    06 01050
      CALL GAMMA(ZR3, 0., GR, GI)                                       06 01060
      BQU = CMPLX(ZR3, 0.)                                              06 01070
      CONU = CONU * 3.33 333 333 333                                    06 01080
      CON2U = 112.5 / RSQSQ / GR / GR / RSQ                             06 01090
      IF (IZTEMP .GT. 0) GOTO 99                                        06 01100
          CONLU = CONLU * 3.33 333 333 333                              06 01110
          COL2U = 270. * PI * (TAZR ** (ZR3 - 3.)) *                    06 01120
     1        (G3P1 + 2.) * CALPHA * (1. - 2.33 333 333 * TAZR /        06 01130
     2        ZR3) / RSQSQ / GR / GR                                    06 01140
   99 RETURN                                                            06 01150
      END                                                               06 01160
      REAL FUNCTION F(W)                                                07 00010
C                                                                       07 00020
C  Fermi function used in calculating B+- spectra and associated        07 00030
C    internal Bremsstrahlung.  Modified to be more consistent with F77  07 00040
C    procedures                                                         07 00050
C                                                                       07 00060
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************07 00070
C*                           Present Code                               07 00080
C*                           ------------                               07 00090
C*  GAMMA                                                               07 00100
C*                           NSDMTH  1(00)                              07 00110
C*                           -------------                              07 00120
C*  HYPERG                                                              07 00130
C*                                                                      07 00140
C*                        FORTRAN 77 Supplied                           07 00150
C*                        -------------------                           07 00160
C*  CEXP    CSQRT   EXP     SQRT                                        07 00170
C*                                                                      07 00180
C***********************************************************************07 00190
      REAL W                                                            07 00200
C                                                                       07 00210
      REAL APOW, BPOW, CALPHA, CM50A, CM50B,                            07 00220
     1    CM80A, CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, 07 00230
     2    G1, G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2, TWOG3, TWOR, V, 07 00240
     3    W0                                                            07 00250
      COMPLEX BQ, BQU                                                   07 00260
      INTEGER IE, IFIN, IZ                                              07 00270
      COMMON /FCOM/ APOW, BPOW, BQ, BQU, CALPHA, CM50A, CM50B, CM80A,   07 00280
     1    CM80B, CM80C, COL2U, CON2U, CONLU, CONU, FRLOW, FRONT, G1,    07 00290
     2    G1P1, G2, G2P1, G3, G3P1, R, TWOG, TWOG2,                     07 00300
     3    TWOG3, TWOR, V, W0, IE, IFIN, IZ                              07 00310
C                                                                       07 00320
      REAL B, BA, F1U, F2U,                                             07 00330
     1    GI, GR, P, PI, PSQ, TWORP,                                    07 00340
     2    W0MW, W0MWSQ, X, XSQ, Y, YOW                                  07 00350
      COMPLEX AQ, B1C, B2C, F1C, G1C, G1Y, G2Y, HGF, HYPERG, QC, XQ     07 00360
C                                                                       07 00370
      REAL EXP,SQRT                                                     07 00380
      COMPLEX CEXP,CMPLX,CSQRT                                          07 00390
      INTRINSIC CEXP,CMPLX,CSQRT,EXP,SQRT                               07 00400
C                                                                       07 00410
      DATA PI/3.14 159 265 359/                                         07 00420
      Save                                                              07 00430
C                                                                       07 00440
C  Neutrino energy is not screened, so calculate it first.              07 00450
      W0MW = W0 - W                                                     07 00460
C  At BETA endpoint function is zero.                                   07 00470
      IF (W0MW .LE. 0.)THEN                                             07 00480
         F = 0.                                                         07 00490
         RETURN                                                         07 00500
      ENDIF                                                             07 00510
      W0MWSQ = W0MW * W0MW                                              07 00520
C  For B-, we simply shift the energy down by V.                        07 00530
      IF (IZ .LE. 0)THEN                                                07 00540
         X = W - V                                                      07 00550
C  For low-E B-, we use a power series approximation.                   07 00560
         IF (X .LE. 1.0001)THEN                                         07 00570
            F = FRLOW * X                                               07 00580
            GOTO 50                                                     07 00590
         ENDIF                                                          07 00600
      ELSE                                                              07 00610
C  For very low-E B+, value is too small to contribute to integral.     07 00620
         IF (W .LT. 1.001)THEN                                          07 00630
            F=0.                                                        07 00640
            RETURN                                                      07 00650
         ENDIF                                                          07 00660
C  B+ screening.                                                        07 00670
         PSQ = W * W - 1.                                               07 00680
         P = SQRT(PSQ)                                                  07 00690
         X = W + V * EXP(APOW / P + BPOW / PSQ)                         07 00700
      ENDIF                                                             07 00710
C-CALCULATE SCREENED MOMENTUM.                                          07 00720
      XSQ = X * X                                                       07 00730
      PSQ = XSQ - 1.                                                    07 00740
      P = SQRT(PSQ)                                                     07 00750
      Y = X * CALPHA / P                                                07 00760
C  The GAMMA function code we use is not recommended above 10.          07 00770
C    So go back to the low E approximation.                             07 00780
      IF (Y .GT. 10.)THEN                                               07 00790
         IF (IZ .GT. 0)THEN                                             07 00800
            F=0.                                                        07 00810
            RETURN                                                      07 00820
         ENDIF                                                          07 00830
         F=FRLOW*X                                                      07 00840
         GOTO 50                                                        07 00850
      ENDIF                                                             07 00860
C  Y must have negative sign for positrons.                             07 00870
      IF (IZ .GT. 0) Y = -Y                                             07 00880
C  Begin calculation of electron wave function.                         07 00890
C    Call for complex gamma function.                                   07 00900
      CALL GAMMA(G1, Y, GR, GI)                                         07 00910
      BA = EXP(PI * Y) / P                                              07 00920
C  B is that portion of the integrand common to both F and G parts      07 00930
C   of the wave function.                                               07 00940
      TWORP = TWOR * P                                                  07 00950
      B = (TWORP ** TWOG) * BA * (GR * GR + GI * GI)                    07 00960
C  Call for Hypergeometric function.                                    07 00970
      XQ = CMPLX(0., TWORP)                                             07 00980
      AQ = CMPLX(G1P1, Y)                                               07 00990
      HGF = HYPERG(AQ, BQ, XQ)                                          07 01000
      YOW = Y / X                                                       07 01010
      QC = CEXP(CMPLX(0., -P * R))                                      07 01020
      G1Y = CMPLX(G1, Y)                                                07 01030
      B1C = QC * G1Y * HGF                                              07 01040
      F1C = B1C * CSQRT(CMPLX(-1., YOW) / G1Y)                          07 01050
      G1C = B1C * CSQRT(CMPLX(1., YOW) / G1Y)                           07 01060
      F = B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +                     07 01070
     1    (X + 1.) * REAL(G1C) * REAL(G1C))                             07 01080
   50 F = F * W0MWSQ                                                    07 01090
C  Correction for finite nuclear size effect.                           07 01100
      IF (IFIN .LT. 0) F = F * (CM50A + X * CM50B)                      07 01110
      IF (IFIN .GT. 0) F = F * (1. + CM80A*X + CM80B/X + CM80C/XSQ)     07 01120
      F = F * FRONT                                                     07 01130
C  For allowed transitions we are finished here.                        07 01140
      IF (IE .LT. 1)RETURN                                              07 01150
C  First forbidden unique.                                              07 01160
C    Check for low-E FFU BETA -.                                        07 01170
      IF ((X .LE. 1.0001) .OR. (Y .GT. 10.))THEN                        07 01180
         F1U = CONLU * X                                                07 01190
      ELSE                                                              07 01200
         CALL GAMMA(G2, Y, GR, GI)                                      07 01210
         B = (TWORP ** TWOG2) * BA * (GR * GR + GI * GI)                07 01220
         AQ = CMPLX(G2P1, Y)                                            07 01230
         HGF = HYPERG(AQ, BQU, XQ)                                      07 01240
         G2Y = CMPLX(G2, Y)                                             07 01250
         B2C = QC * G2Y * HGF                                           07 01260
         F1C = B2C * CSQRT(CMPLX(-2., YOW) / G2Y)                       07 01270
         G1C = B2C * CSQRT(CMPLX(2., YOW) / G2Y)                        07 01280
         F1U = CONU * B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +         07 01290
     1     (X + 1.) * REAL(G1C) * REAL(G1C))                            07 01300
      ENDIF                                                             07 01310
      F = W0MWSQ * (F + F1U)                                            07 01320
      IF (IE .LE. 1)RETURN                                              07 01330
C  Second forbidden unique.                                             07 01340
      IF ((X .LE. 1.0001) .OR. (Y .GT. 10.))THEN                        07 01350
         F2U = COL2U * X                                                07 01360
      ELSE                                                              07 01370
         CALL GAMMA(G3, Y, GR, GI)                                      07 01380
         B = (TWORP ** TWOG3) * BA * (GR * GR + GI * GI)                07 01390
         AQ = CMPLX(G3P1, Y)                                            07 01400
         HGF = HYPERG(AQ, BQU, XQ)                                      07 01410
         G2Y = CMPLX(G3, Y)                                             07 01420
         B2C = QC * G2Y * HGF                                           07 01430
         F1C = B2C * CSQRT(CMPLX(-3., YOW) / G2Y)                       07 01440
         G1C = B2C * CSQRT(CMPLX(3., YOW) / G2Y)                        07 01450
         F2U = CON2U * B * ((X - 1.) * AIMAG(F1C) * AIMAG(F1C) +        07 01460
     1     (X + 1.) * REAL(G1C) * REAL(G1C))                            07 01470
      ENDIF                                                             07 01480
      F = W0MWSQ * (F + F2U)                                            07 01490
      RETURN                                                            07 01500
      END                                                               07 01510
      SUBROUTINE GAMMA(XR, XI, ZR, ZI)                                  08 00010
      REAL XR, XI, ZR, ZI                                               08 00020
      COMPLEX X, Z, GAMA                                                08 00030
      X = CMPLX(XR, XI)                                                 08 00040
      Z = GAMA(X)                                                       08 00050
      ZR = REAL(Z)                                                      08 00060
      ZI = AIMAG(Z)                                                     08 00070
      RETURN                                                            08 00080
      END                                                               08 00090
      REAL FUNCTION CAPTUR                                              09 00010
     1    (IZ, W0, IPRINT, ITYPE, FCK, FCL, FCMNO)                      09 00020
C                                                                       09 00030
      INTEGER IZ, IPRINT, ITYPE                                         09 00040
      REAL W0, FCK, FCL, FCMNO                                          09 00050
C                                                                       09 00060
      REAL BKL(127), BL1L(128), BL2L(128), BL3L(128)                    09 00070
      REAL WM1L(128), WM3L(128)                                         09 00080
C                                                                       09 00090
      REAL BK, BL1, BL2, BL3                                            09 00100
C      REAL EKTOE, EKTOEU, ELTKU, ELTOEK, EMNOTL, EMNTLU                09 00110
      REAL ELTOT, ELTOTU                                                09 00120
      REAL FL2SQ                                                        09 00130
      REAL GL1SQ, GL3R9                                                 09 00140
      REAL PCAP, PCAPU, PKEX, PKU, PL1EX, PL1U, PL2, PL2U,              09 00150
     1    PL3U, PM3, PMN, PMNU, PN3, PSIKSQ                             09 00160
C      REAL R2TL1, R2TL1U, R3TL1U,                                      09 00170
      REAL RM3, RMNO, RN3                                               09 00180
      REAL WE, WEK, WEKSQ, WEL1, WEL1SQ, WEL2, WEL2SQ,                  09 00190
     1    WEL3, WEL3SQ, WEM3, WEM3SQ, WEMNO, WEMSQ, WEN3, WEN3SQ,       09 00200
     2    WK, WL1, WL2, WL3                                             09 00210
      REAL Z                                                            09 00220
C                                                                       09 00230
      INTEGER IZEFF                                                     09 00240
C                                                                       09 00250
C     Corrections made for Z<4.  Values taken from Appendix III of Table09 00260
C       of Isotopes, Seventh Edition.  880413                           09 00270
      DATA  BKL/ 0.014E-3,0.0246E-3,0.0000548,0.000112,0.000188,        09 00280
     1 0.000284,0.0004,                                                 09 00290
     1 0.000532,0.00068,                                                09 00300
     1 .0008669,.0010721,.0013050,.0015596,.0018389,.0021455,.0024720,  09 00310
     1 .0028224,.0032029,.0036074,.0040381,.0044928,.0049664,.0054651,  09 00320
     1 .0059892,.0065390,.0071120,.0077089,.0083328,.0089789,.0096586,  09 00330
     1 .0103671,.0111031,.0118667,.0126578,.0134737,.0143256,.0151997,  09 00340
     1 .0161046,.0170384,.0179976,.0189856,.0199995,.0210440,.0221172,  09 00350
     1 .0232199,.0243503,.0255140,.0267112,.0279399,.0292001,.0304912,  09 00360
     1 .0318138,.0331694,.0345614,.0359846,.0374406,.0389246,.0404430,  09 00370
     1 .0419906,.0435689,.0451840,.0468342,.0485190,.0502391,.0519957,  09 00380
     1 .0537885,.0556177,.0574855,.0593896,.0613323,.0633138,.0653508,  09 00390
     1 .0674164,.0695250,.0716764,.0738708,.0761110,.0783948,.0807249,  09 00400
     1 .0831023,.0855304,.0880045,.0905259,.0931050,.0957299,.098404 ,  09 00410
     1 .101137 ,.1039219,.1067553,.1096509,.1126014,.1156061,.118678 ,  09 00420
     1 .121818 ,.125027 ,.128220 ,.131590 ,.135960 ,.139490 ,.143090 ,  09 00430
     1.146780,.15054, 25 * .160/                                        09 00440
C     Values added for Z=3 and 4 from same source.  880413              09 00450
      DATA  BL1L/ 2 * 0.0,0.001E-3,0.003E-3,5 * 0.,                     09 00460
     1 .000045 ,.0000633,.0000894,.0001177,.0001487,.0001893,.0002292,  09 00470
     1 .0002702,.000320 ,.0003771,.0004378,.0005004,.0005637,.0006282,  09 00480
     1 .0006946,.0007690,.0008461,.0009256,.0010081,.0010966,.0011936,  09 00490
     1 .0012977,.0014143,.0015265,.0016539,.0017820,.0019210,.0020651,  09 00500
     1 .0022163,.0023725,.0025316,.0026977,.0028655,.0030425,.0032240,  09 00510
     1 .0034119,.0036043,.0038058,.0040180,.0042375,.0044647,.0046983,  09 00520
     1 .0049392,.0051881,.0054528,.0057143,.0059888,.0062663,.0065488,  09 00530
     1 .0068348,.0071260,.0074279,.0077368,.0080520,.0083756,.0087080,  09 00540
     1 .0090458,.0093942,.0097513,.0101157,.0104864,.0108704,.0112707,  09 00550
     1 .0116815,.0120998,.0125267,.0129680,.0134185,.0138799,.0143528,  09 00560
     1 .0148393,.0153467,.0158608,.0163875,.0169393,.017493 ,.018049 ,  09 00570
     1 .018639 ,.0192367,.019840 ,.0204721,.0211046,.0217574,.0224268,  09 00580
     1 .0230972,.0237729,.024460 ,.025275 ,.026110 ,.026900 ,.027700 ,  09 00590
     1 .028530 ,.029380 ,.030240 , 25 * 0.031/                          09 00600
      DATA  BL2L/ 9 * 0.,                                               09 00610
     1 .0000183,.0000311,.0000514,.0000731,.0000992,.0001322,.0001648,  09 00620
     1 .0002016,.0002473,.0002963,.0003500,.0004067,.0004615,.0005205,  09 00630
     1 .0005837,.0006514,.0007211,.0007936,.0008719,.0009510,.0010428,  09 00640
     1 .0011423,.0012478,.0013586,.0014762,.0015960,.0017272,.0018639,  09 00650
     1 .0020068,.0021555,.0023067,.0024647,.0026251,.0027932,.0029669,  09 00660
     1 .0031461,.0033303,.0035237,.0037270,.0039380,.0041561,.0043804,  09 00670
     1 .0046120,.0048521,.0051037,.0053594,.0056236,.0058906,.0061642,  09 00680
     1 .0064404,.0067215,.0070128,.0073118,.0076171,.0079303,.0082516,  09 00690
     1 .0085806,.0089178,.0092643,.0096169,.0099782,.0103486,.0107394,  09 00700
     1 .0111361,.0115440,.0119587,.0123850,.0128241,.0132726,.0137336,  09 00710
     1 .0142087,.0146979,.0152000,.0157111,.0162443,.0167847,.0173371,  09 00720
     1 .0179065,.0184843,.0190832,.0196932,.0203137,.0209476,.0216005,  09 00730
     1 .0222662,.0229440,.023779 ,.024385 ,.025250 ,.026020 ,.026810 ,  09 00740
     1 .027610 ,.028440 ,.029280 , 25 * 0.030/                          09 00750
      DATA  BL3L/ 9 * 0.,                                               09 00760
     1 .0000183,.0000311,.0000514,.0000731,.0000992,.0001322,.0001648,  09 00770
     1 .0002000,.0002452,.0002936,.0003464,.0004022,.0004555,.0005129,  09 00780
     1 .0005745,.0006403,.0007081,.0007786,.0008547,.0009311,.0010197,  09 00790
     1 .0011154,.0012167,.0013231,.0014358,.0015499,.0016749,.0018044,  09 00800
     1 .0019396,.0020800,.0022223,.0023705,.0025202,.0026769,.0028379,  09 00810
     1 .0030038,.0031733,.0033511,.0035375,.0037301,.0039288,.0041322,  09 00820
     1 .0043414,.0045571,.0047822,.0050119,.0052470,.0054827,.0057234,  09 00830
     1 .0059643,.0062079,.0064593,.0067162,.0069769,.0072428,.0075140,  09 00840
     1 .0077901,.0080711,.0083579,.0086480,.0089436,.0092441,.0095607,  09 00850
     1 .0098811,.0102068,.0105353,.0108709,.0112152,.0115637,.0119187,  09 00860
     1 .0122839,.0126575,.0130352,.0134186,.0138138,.0142135,.0146194,  09 00870
     1 .0150312,.0154444,.0158710,.0163003,.0167331,.0171663,.0176100,  09 00880
     1 .0180568,.0185041,.018930 ,.019452 ,.019930 ,.020410 ,.020900 ,  09 00890
     1 .021390 ,.021880 ,.022360 , 25 * 0.025/                          09 00900
      DATA WM1L/9*1.00,                                                 09 00910
     1 1.000000,1.000000,1.000000,1.000000,1.000000,0.999992,0.999980,  09 00920
     1 0.999966,0.999950,0.999934,0.999914,0.999895,0.999882,0.999870,  09 00930
     1 0.999855,0.999836,0.999818,0.999803,0.999781,0.999766,0.999734,  09 00940
     1 0.999691,0.999648,0.999602,0.999547,0.999498,0.999436,0.999370,  09 00950
     1 0.999300,0.999230,0.999158,0.999083,0.999013,0.998933,0.998855,  09 00960
     1 0.998773,0.998689,0.998596,0.998493,0.998384,0.998270,0.998153,  09 00970
     1 0.998031,0.997902,0.997759,0.997618,0.997470,0.997336,0.997193,  09 00980
     1 0.997043,0.996917,0.996781,0.996629,0.996477,0.996319,0.996150,  09 00990
     1 0.995995,0.995835,0.995682,0.995486,0.995307,0.995125,0.994910,  09 01000
     1 0.994701,0.994482,0.994263,0.994034,0.993789,0.993550,0.993298,  09 01010
     1 0.993030,0.992751,0.992464,0.992174,0.991880,0.991552,0.991229,  09 01020
     1 0.990896,0.990564,0.990211,0.989859,0.989497,0.989143,0.988800,  09 01030
     1 0.988390,0.988023,0.987695,0.987170,0.986783,0.986346,0.985900,  09 01040
     1 0.985438,0.984980,0.984540,25*0.983/                             09 01050
      DATA WM3L/9*1.00,                                                 09 01060
     1 1.000000,1.000000,1.000000,1.000000,1.000000,1.000000,0.999997,  09 01070
     1 0.999987,0.999976,0.999965,0.999950,0.999937,0.999932,0.999926,  09 01080
     1 0.999917,0.999905,0.999894,0.999884,0.999867,0.999856,0.999831,  09 01090
     1 0.999799,0.999764,0.999725,0.999683,0.999645,0.999582,0.999533,  09 01100
     1 0.999473,0.999412,0.999353,0.999290,0.999232,0.999168,0.999099,  09 01110
     1 0.999029,0.998960,0.998882,0.998794,0.998700,0.998602,0.998502,  09 01120
     1 0.998398,0.998288,0.998166,0.998048,0.997921,0.997802,0.997680,  09 01130
     1 0.997569,0.997461,0.997345,0.997222,0.997103,0.996978,0.996847,  09 01140
     1 0.996721,0.996593,0.996454,0.996312,0.996184,0.996040,0.995876,  09 01150
     1 0.995706,0.995536,0.995367,0.995191,0.995008,0.994823,0.994632,  09 01160
     1 0.994428,0.994214,0.993999,0.993783,0.993538,0.993295,0.993076,  09 01170
     1 0.992832,0.992580,0.992350,0.992082,0.991832,0.991578,0.991322,  09 01180
     1 0.991083,0.990867,0.990613,0.990260,0.990002,0.989722,0.989438,  09 01190
     1 0.989147,0.988869,0.988826,25*0.99/                              09 01200
      Save                                                              09 01210
C                                                                       09 01220
C-TO CALCULATE THE ELECTRON CAPTURE CONTRIBUTION TO THE LOG FT.         09 01230
C                                                                       09 01240
C-CAPTUR MUST BE CALLED FOR ALLOWED CASE BEFORE CALLING FOR UNIQUE.     09 01250
C                                                                       09 01260
C-WE USE BINDING ENERGIES OF DAUGHTER, WAVE FUNCTIONS OF PARENT.        09 01270
C                                                                       09 01280
      IZEFF = IZ + 1                                                    09 01290
      WE = W0 - 2.                                                      09 01300
      BK = BKL(IZ)                                                      09 01310
      BL1 = BL1L(IZ)                                                    09 01320
      BL2 = BL2L(IZ)                                                    09 01330
      BL3 = BL3L(IZ)                                                    09 01340
      WK = 1. - BK / 0.511                                              09 01350
      WL1 = 1. - BL1 / 0.511                                            09 01360
      WL2 = 1. - BL2 / 0.511                                            09 01370
      WL3 = 1. - BL3 / 0.511                                            09 01380
      WEK = WE + WK                                                     09 01390
      WEKSQ = WEK * WEK                                                 09 01400
      WEL1  = WE + WL1                                                  09 01410
      WEL1SQ= WEL1 * WEL1                                               09 01420
      WEL2  = WE + WL2                                                  09 01430
      WEL2SQ= WEL2 * WEL2                                               09 01440
      WEMNO = WE + WM1L(IZ)                                             09 01450
      WEMSQ = WEMNO * WEMNO                                             09 01460
C                                                                       09 01470
C-GET WAVE FUNCTIONS.                                                   09 01480
C                                                                       09 01490
      CALL WAVEFN(IZEFF, PSIKSQ, GL1SQ, FL2SQ, GL3R9, RMNO)             09 01500
C                                                                       09 01510
C-K-CAPTURE PORTION.                                                    09 01520
C                                                                       09 01530
C-TEST IF ENOUGH ENERGY FOR K-CAPTURE.                                  09 01540
C                                                                       09 01550
      PKEX = 0.                                                         09 01560
      IF (WEK .GT. 0.) PKEX = WEKSQ * PSIKSQ                            09 01570
C                                                                       09 01580
C-L-CAPTURE PORTION.                                                    09 01590
C                                                                       09 01600
      PL1EX = 0.                                                        09 01610
      IF (WEL1 .GT. 0.) PL1EX = WEL1SQ * GL1SQ                          09 01620
      PL2 = 0.                                                          09 01630
      IF (WEL2 .GT. 0.) PL2 = WEL2SQ * FL2SQ                            09 01640
C                                                                       09 01650
      IF (WEMNO .LE. 0.) GOTO 500                                       09 01660
      PMN = WEMSQ * RMNO                                                09 01670
C                                                                       09 01680
C-BRANCH IF UNIQUE TRANSITION.                                          09 01690
C                                                                       09 01700
      IF (ITYPE .NE. 0) GOTO 360                                        09 01710
C                                                                       09 01720
C-SUMMARIZE CAPTURE COMPONENTS.                                         09 01730
C                                                                       09 01740
          ELTOT = PL1EX + PL2                                           09 01750
          PCAP = PKEX + ELTOT + PMN                                     09 01760
          CAPTUR = 1.5708 * PCAP                                        09 01770
          FCK = 1.5708 * PKEX                                           09 01780
          FCL = 1.5708 * ELTOT                                          09 01790
          FCMNO = 1.5708 * PMN                                          09 01800
C                                                                       09 01810
C-PRINTING (OPTIONAL).                                                  09 01820
C                                                                       09 01830
          IF (IPRINT .LE. 0) GOTO 99                                    09 01840
          IF (IPRINT .LT. 2) GOTO 285                                   09 01850
              WRITE(36, 6000) IZ, BK, BL1, BL2, BL3                     09 01860
 6000         FORMAT(' BINDING ENERGIES (DAUGHTER)',                    09 01870
     1            5X, 'Z=', I5, 10X, 'K', F8.4, 10X, 'L1', F8.4, 10X,   09 01880
     2            'L2', F8.4, 10X, 'L3', F8.4)                          09 01890
              WRITE(36, 6010) IZEFF, PSIKSQ, GL1SQ, FL2SQ, GL3R9        09 01900
 6010         FORMAT(' WAVE FUNCTIONS, PARENT.  Z=', I4, 8X, 'K',       09 01910
     1            E10.2, 8X, 'L1', E10.2, 8X, 'L2', E10.2, 8X,          09 01920
     2            '9*L3/R*R', E10.2)                                    09 01930
  285     IF (PKEX .GT. 0.) GOTO 320                                    09 01940
              WRITE(36, 6020)                                           09 01950
 6020         FORMAT(20X, 'ENERGY TOO LOW FOR K-CAPTURE')               09 01960
              IF (PL1EX .GT. 0.) GOTO 330                               09 01970
              WRITE(36, 6030)                                           09 01980
 6030         FORMAT(20X, 'ENERGY TOO LOW FOR L1 CAPTURE')              09 01990
              IF (PL2 .GT. 0.) GOTO 340                                 09 02000
              WRITE(36, 6040)                                           09 02010
 6040         FORMAT(20X, 'ENERGY TOO LOW FOR L2 CAPTURE')              09 02020
              GOTO 99                                                   09 02030
C                                                                       09 02040
  320     CONTINUE                                                      09 02050
C         EKTOE = PKEX / PCAP                                           09 02060
C         ELTOEK = ELTOT / PKEX                                         09 02070
C         WRITE(36, 6050) EKTOE, ELTOEK                                 09 02080
C6050     FORMAT(20X, 'K-CAPTURE TO TOTAL CAPTURE RATIO = ', F7.5,      09 02090
C    1        10X, ' L/K CAPTURE RATIO', F10.4)                         09 02100
C                                                                       09 02110
  330     CONTINUE                                                      09 02120
C         R2TL1 = PL2 / PL1EX                                           09 02130
C         WRITE(36, 6060) R2TL1                                         09 02140
C6060     FORMAT(20X, 'L2/L1 CAPTURE RATIO = ', E10.3)                  09 02150
C                                                                       09 02160
  340     CONTINUE                                                      09 02170
C         EMNOTL = PMN / ELTOT                                          09 02180
C         WRITE(36, 6070) EMNOTL                                        09 02190
C6070     FORMAT(20X, 'MNO-CAPTURE RATIO TO L-CAPTURE RATIO = ', E10.3) 09 02200
          GOTO 99                                                       09 02210
C                                                                       09 02220
C-FIRST FORBIDDEN UNIQUE CAPTURE.                                       09 02230
C                                                                       09 02240
  360 PKU = PKEX * WEKSQ                                                09 02250
      PL1U = PL1EX * WEL1SQ                                             09 02260
      PL2U = PL2 * WEL2SQ                                               09 02270
C                                                                       09 02280
C-L3 TERM.                                                              09 02290
C                                                                       09 02300
      WEL3 = WE + WL3                                                   09 02310
      WEL3SQ = WEL3 * WEL3                                              09 02320
      PL3U = 0.                                                         09 02330
      IF (WEL3 .GT. 0.) PL3U = WEL3SQ * GL3R9                           09 02340
C                                                                       09 02350
C-MNO CAPTURE.                                                          09 02360
C                                                                       09 02370
      PMNU = PMN * WEMSQ                                                09 02380
      PM3 = 0.                                                          09 02390
      PN3 = 0.                                                          09 02400
      IF (IZ .LT. 18) GOTO 394                                          09 02410
          WEM3 = WE + WM3L(IZ)                                          09 02420
          WEM3SQ = WEM3 * WEM3                                          09 02430
          Z = IZ                                                        09 02440
          RM3 = 4.3E-2 + Z * (4.198E-3 - Z * 1.6E-5)                    09 02450
          PM3 = WEM3SQ * GL3R9 * RM3                                    09 02460
          IF (IZ .LT. 36) GOTO 394                                      09 02470
              RN3 = -5.27E-3 + Z * (2.352E-3 - Z * 9.85E-6)             09 02480
              WEN3 = WE + 1.                                            09 02490
              WEN3SQ = WEN3 * WEN3                                      09 02500
              PN3 = WEN3SQ * GL3R9 * RN3                                09 02510
  394 IF (ITYPE .EQ. 1) GOTO 395                                        09 02520
C                                                                       09 02530
C-SECOND FORBIDDEN UNIQUE.                                              09 02540
C                                                                       09 02550
          PKU = PKU * WEKSQ                                             09 02560
          PL1U = PL1U * WEL1SQ                                          09 02570
          PL2U = PL2U * WEL2SQ                                          09 02580
          PMNU = PMNU * WEMSQ                                           09 02590
          PL3U = PL3U * WEL3SQ * 3.33 333 333                           09 02600
          PM3 = PM3 * WEM3SQ * 3.33 333 333                             09 02610
          PN3 = PN3 * WEN3SQ * 3.33 333 333                             09 02620
C                                                                       09 02630
C-SUMMARIZE UNIQUE CAPTURE.                                             09 02640
C                                                                       09 02650
  395 ELTOTU = PL1U + PL2U + PL3U                                       09 02660
      PMNU = PMNU + PM3 + PN3                                           09 02670
      PCAPU = PKU + ELTOTU + PMNU                                       09 02680
      CAPTUR = 1.5708 * PCAPU                                           09 02690
      FCK = 1.5708 * PKU                                                09 02700
      FCL = 1.5708 * ELTOTU                                             09 02710
      FCMNO = 1.5708 * PMNU                                             09 02720
C                                                                       09 02730
C-PRINTING (OPTIONAL).                                                  09 02740
C                                                                       09 02750
      IF (IPRINT .LE. 0) GOTO 99                                        09 02760
      IF (PKEX .GT. 0.) GOTO 430                                        09 02770
          IF (PL3U .GT. 0.) GOTO 440                                    09 02780
              WRITE(36, 6080)                                           09 02790
 6080         FORMAT(20X, 'ENERGY TOO LOW FOR L3 CAPTURE ')             09 02800
              GOTO 460                                                  09 02810
C                                                                       09 02820
  430 CONTINUE                                                          09 02830
C     EKTOEU = PKU / PCAPU                                              09 02840
C     ELTKU = ELTOTU / PKU                                              09 02850
C     WRITE(36, 6050) EKTOEU, ELTKU                                     09 02860
  440 IF (PL1U .LE. 0.) GOTO 460                                        09 02870
C     R2TL1U = PL2U / PL1U                                              09 02880
C         WRITE(36, 6060) R2TL1U                                        09 02890
C         R3TL1U = PL3U / PL1U                                          09 02900
C         WRITE(36, 6090) R3TL1U                                        09 02910
C6090     FORMAT(20X, 'L3/L1 CAPTURE RATIO = ', E10.3)                  09 02920
  460 CONTINUE                                                          09 02930
C     EMNTLU = PMNU / ELTOTU                                            09 02940
C     WRITE(36, 6070) EMNTLU                                            09 02950
      GOTO 99                                                           09 02960
C                                                                       09 02970
  500 WRITE(36, 7000)                                                   09 02980
 7000 FORMAT(' ENERGY TOO LOW FOR THIS PROGRAM')                        09 02990
      CAPTUR = 0.                                                       09 03000
      FCK = 0.                                                          09 03010
      FCL = 0.                                                          09 03020
      FCMNO = 0.                                                        09 03030
C                                                                       09 03040
   99 RETURN                                                            09 03050
      END                                                               09 03060
      BLOCK DATA                                                        10 00010
C                                                                       10 00020
      REAL GKL(130)                                                     10 00030
      REAL GLUL(130)                                                    10 00040
      REAL FLUL(130)                                                    10 00050
      REAL GL3L(130)                                                    10 00060
      REAL RL(130)                                                      10 00070
C                                                                       10 00080
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         10 00090
C                                                                       10 00100
      DATA GKL, GLUL, FLUL, GL3L, RL/650 * 0.0/                         10 00110
C                                                                       10 00120
      END                                                               10 00130
      SUBROUTINE READWV                                                 11 00010
C                                                                       11 00020
C  READS WAVE FUNCTION DATA FOR USE BY WAVEFN.                          11 00030
C                                                                       11 00040
      REAL GKL(130), GK                                                 11 00050
      REAL GLUL(130), GL1                                               11 00060
      REAL FLUL(130), FL2                                               11 00070
      REAL GL3L(130), GL3                                               11 00080
      REAL RL(130), RMNO                                                11 00090
C                                                                       11 00100
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         11 00110
C                                                                       11 00120
      INTEGER IZ                                                        11 00130
C                                                                       11 00140
C     Radial wave functions for Z<5 added to LOGFT.DAT as per JKT's     11 00150
C       phone talks with MJM 890413                                     11 00160
C       It is not clear whether GK's include or GL1/GK=0.031 for Z=4 inc11 00170
C       exchange corrections                                            11 00180
      Save                                                              11 00190
C                                                                       11 00200
    1 READ(20, 100) IZ, GK, GL1, FL2, GL3, RMNO                         11 00210
  100 FORMAT(I3, 7X, 5F10.0)                                            11 00220
C                                                                       11 00230
      IF (IZ .LE. 0) RETURN                                             11 00240
          GKL(IZ) = GK                                                  11 00250
          GLUL(IZ) = GL1                                                11 00260
          FLUL(IZ) = FL2                                                11 00270
          GL3L(IZ) = GL3                                                11 00280
          RL(IZ) = RMNO                                                 11 00290
          GOTO 1                                                        11 00300
C                                                                       11 00310
      END                                                               11 00320
      SUBROUTINE WAVEFN(IZ, GK, GLU, FLU, GL3, RMNO)                    12 00010
C                                                                       12 00020
C  RETURNS WAVE FUNCTION DATA VALUES.                                   12 00030
C                                                                       12 00040
      REAL GKL(130), GK                                                 12 00050
      REAL GLUL(130), GLU                                               12 00060
      REAL FLUL(130), FLU                                               12 00070
      REAL GL3L(130), GL3                                               12 00080
      REAL RL(130), RMNO                                                12 00090
      REAL DG, DL, DF, DGL, DR                                          12 00100
C                                                                       12 00110
      COMMON /WAVCOM/ GKL, GLUL, FLUL, GL3L, RL                         12 00120
C                                                                       12 00130
      INTEGER I, IZ, IM1, IP1, IN, INM1                                 12 00140
      Save                                                              12 00150
C                                                                       12 00160
C-INITIALIZATION.                                                       12 00170
C                                                                       12 00180
   10 GK = GKL(IZ)                                                      12 00190
      GLU = GLUL(IZ)                                                    12 00200
      FLU = FLUL(IZ)                                                    12 00210
      GL3 = GL3L(IZ)                                                    12 00220
      RMNO = RL(IZ)                                                     12 00230
C                                                                       12 00240
C-EXTRAPOLATION FOR MISSING DATA.                                       12 00250
C                                                                       12 00260
      IF (GK .NE. 0.) RETURN                                            12 00270
C                                                                       12 00280
C-FIND NEAREST LIBRARY VALUES.                                          12 00290
C                                                                       12 00300
      I = IZ                                                            12 00310
   20 I = I - 1                                                         12 00320
      IF (GKL(I) .EQ. 0.) GOTO 20                                       12 00330
      IM1 = I - 1                                                       12 00340
      DG = GKL(I) - GKL(IM1)                                            12 00350
      DL = GLUL(I) - GLUL(I - 1)                                        12 00360
      DF = FLUL(I) - FLUL(IM1)                                          12 00370
      DGL = GL3L(I) - GL3L(IM1)                                         12 00380
      DR = RL(I) - RL(IM1)                                              12 00390
      IP1 = I + 1                                                       12 00400
      DO 30 IN = IP1, IZ                                                12 00410
          INM1 = IN - 1                                                 12 00420
          GKL(IN) = GKL(INM1) + DG                                      12 00430
          GLUL(IN) = GLUL(INM1) + DL                                    12 00440
          FLUL(IN) = FLUL(INM1) + DF                                    12 00450
          GL3L(IN) = GL3L(INM1) + DGL                                   12 00460
          RL(IN) = RL(INM1) + DR                                        12 00470
   30     CONTINUE                                                      12 00480
      GOTO 10                                                           12 00490
      END                                                               12 00500
      INTEGER FUNCTION IPREC(STR)                                       13 00010
C                                                                       13 00020
C     Corrected subroutine to account for floating point notation       13 00030
C       (TWB. 921216)                                                   13 00040
C     Improved algorithms for calculating precision (TWB. 930212)       13 00050
C                                                                       13 00060
      CHARACTER*(*) STR                                                 13 00070
C                                                                       13 00080
      INTEGER I,IEND,IPER                                               13 00090
      CHARACTER*10  LOCAL                                               13 00100
C                                                                       13 00110
      INTEGER INDEXF,LENSTR                                             13 00120
      EXTERNAL INDEXF,LENSTR                                            13 00130
      INTEGER INDEX                                                     13 00140
      INTRINSIC INDEX                                                   13 00150
C                                                                       13 00160
      LOCAL=STR                                                         13 00170
      CALL SQZSTR(LOCAL, ' ')                                           13 00180
      IPER = INDEX(LOCAL, '.')                                          13 00190
      IF (IPER .GT. 0)THEN                                              13 00200
         IEND=INDEXF(LOCAL,IPER,'E')-1                                  13 00210
         IF(IEND .LE. 0)IEND=LENSTR(LOCAL)                              13 00220
         IPREC = IEND - IPER                                            13 00230
         IF(IPER .GT. 2)THEN                                            13 00240
            IPREC=IPREC+IPER-1                                          13 00250
         ELSE                                                           13 00260
            IF(IPER .EQ. 2)THEN                                         13 00270
               IF(LOCAL(1:1) .NE. '0')THEN                              13 00280
                  IPREC=IPREC+1                                         13 00290
               ELSE                                                     13 00300
                  DO 100 I=IPER+1,IEND                                  13 00310
                     IF(LOCAL(I:I) .NE. '0')GOTO 200                    13 00320
                     IPREC=IPREC-1                                      13 00330
100               CONTINUE                                              13 00340
200               CONTINUE                                              13 00350
               ENDIF                                                    13 00360
            ENDIF                                                       13 00370
         ENDIF                                                          13 00380
      ELSE                                                              13 00390
         IEND=INDEX(LOCAL,'E')                                          13 00400
         IF(IEND .LE. 0)THEN                                            13 00410
            IPREC=LENSTR(LOCAL)                                         13 00420
         ELSE                                                           13 00430
            IPREC=IEND-1                                                13 00440
         ENDIF                                                          13 00450
      ENDIF                                                             13 00460
      RETURN                                                            13 00470
      END                                                               13 00480
      SUBROUTINE UNCALP(DX,UNCERT,IFIELD)                               14 00010
C                                                                       14 00020
C   If UNCERT is the given non-numeric uncertainty then decide          14 00030
C   what the correct non-numeric uncertainty for the field IFIELD       14 00040
C   is.  And Returns the uncertainty in DX.                             14 00050
C     IFIELD=1   OUTPUTFIELD LOGFT                                      14 00060
C            4               IB                                         14 00070
C            5               IE                                         14 00080
C                                                                       14 00090
      INTEGER IFIELD                                                    14 00100
      CHARACTER*2 UNCERT                                                14 00110
      CHARACTER*2 DX                                                    14 00120
C                                                                       14 00130
      IF(UNCERT.EQ.'AP' .OR. UNCERT.EQ.'CA' .OR. UNCERT.EQ.'SY') THEN   14 00140
         DX=UNCERT                                                      14 00150
         RETURN                                                         14 00160
      ENDIF                                                             14 00170
C                                                                       14 00180
C   IFIELD=1   LOGFT                                                    14 00190
C                                                                       14 00200
      IF(IFIELD.EQ.1) THEN                                              14 00210
         IF(UNCERT(1:1).EQ.'L') THEN                                    14 00220
            DX='G'//UNCERT(2:2)                                         14 00230
         ELSE                                                           14 00240
            DX='L'//UNCERT(2:2)                                         14 00250
         ENDIF                                                          14 00260
         RETURN                                                         14 00270
      ENDIF                                                             14 00280
C                                                                       14 00290
C   IFIELD=4 OR 5   IB OR IE                                            14 00300
C                                                                       14 00310
      IF(IFIELD.EQ.4 .OR. IFIELD.EQ.5) THEN                             14 00320
         DX=UNCERT                                                      14 00330
         RETURN                                                         14 00340
      ENDIF                                                             14 00350
      RETURN                                                            14 00360
      END                                                               14 00370
      SUBROUTINE CENTER(STR,IFIELD)                                     15 00010
C                                                                       15 00020
C   Take STR and center its characters in STR(1:IFIELD)                 15 00030
C                                                                       15 00040
      CHARACTER*(*) STR                                                 15 00050
      INTEGER       IFIELD                                              15 00060
C                                                                       15 00070
      INTEGER I,LAST                                                    15 00080
C                                                                       15 00090
      INTEGER LENSTR                                                    15 00100
      EXTERNAL LENSTR                                                   15 00110
C                                                                       15 00120
      CALL LBSUP(STR)                                                   15 00130
      LAST=LENSTR(STR)                                                  15 00140
      IF(LAST.GE.IFIELD) RETURN                                         15 00150
      I=IFIELD - (IFIELD-LAST)/2                                        15 00160
      CALL PADLFT(STR,I)                                                15 00170
      RETURN                                                            15 00180
      END                                                               15 00190
        REAL FUNCTION FNEW(X)                                           16 00010
C                                                                       16 00020
C  Short form of the Fermi function used for B+- and associated IB.     16 00030
C    This function attempts to obtain the Fermi function by polynomial  16 00040
C    interpolation.  If the interpolation is not successful, the value  16 00050
C    is obtained from FUNCTION F and stored in FSAV for later           16 00060
C    interpolations.  The parameter N should be reset to 1 with         16 00070
C    each call to SETUP.                                                16 00080
C                                                                       16 00090
C***************** SUBROUTINES AND FUNCTIONS CALLED ********************16 00100
C*                           Present Code                               16 00110
C*                           ------------                               16 00120
C*  F       LOCATE  POLINT                                              16 00130
C*                                                                      16 00140
C*                        FORTRAN 77 Supplied                           16 00150
C*                        -------------------                           16 00160
C*  ABS                                                                 16 00170
C*                                                                      16 00180
C***********************************************************************16 00190
        REAL X                                                          16 00200
C                                                                       16 00210
        INTEGER NSAV,NSET,NSTEPM,NTEST                                  16 00220
C        PARAMETER (NSAV=200,NSET=3,NSTEPM=6)                           16 00230
        PARAMETER (NSAV=200,NSET=3,NSTEPM=6,NTEST=NSAV/10)              16 00240
C                                                                       16 00250
        REAL F                                                          16 00260
        EXTERNAL F                                                      16 00270
      REAL ABS,ALOG10                                                   16 00280
      DOUBLE PRECISION DABS,DBLE                                        16 00290
      INTRINSIC ABS,ALOG10,DABS,DBLE                                    16 00300
C                                                                       16 00310
        REAL DUM,EPREC                                                  16 00320
        COMMON/PREC1/DUM,EPREC                                          16 00330
C                                                                       16 00340
        INTEGER N                                                       16 00350
        COMMON /FCOMM/N                                                 16 00360
C                                                                       16 00370
        INTEGER I,J,JJ,LOWER,NSTEP,THPCK                                16 00380
        REAL DY,TEMP1,TEMP2,Y,ESAV(NSAV),FSAV(NSAV)                     16 00390
C                                                                       16 00400
      LOGICAL ISLOW,ISHIG                                               16 00410
      REAL LOWZER,HGHZER                                                16 00420
      Save                                                              16 00430
C                                                                       16 00440
      IF(N .EQ. 1)THEN                                                  16 00450
         ISLOW=.FALSE.                                                  16 00460
         ISHIG=.FALSE.                                                  16 00470
      ELSE                                                              16 00480
         IF(ISLOW)THEN                                                  16 00490
            IF(X .LE. LOWZER)THEN                                       16 00500
               FNEW=0.                                                  16 00510
               RETURN                                                   16 00520
            ENDIF                                                       16 00530
         ENDIF                                                          16 00540
         IF(ISHIG)THEN                                                  16 00550
            IF(X .GE. HGHZER)THEN                                       16 00560
               FNEW=0.                                                  16 00570
               RETURN                                                   16 00580
            ENDIF                                                       16 00590
         ENDIF                                                          16 00600
      ENDIF                                                             16 00610
        IF(N .GE. NSET .AND. X .LE. ESAV(N-1))THEN                      16 00620
           CALL LOCATE(ESAV,N-1,X,LOWER)                                16 00630
C          Changed comparison to double precision (TWB. 930112)         16 00640
           IF(LOWER .EQ. 0 .AND. ABS(ALOG10(X)-ALOG10(ESAV(1))) .LE. 1.)16 00650
     2       THEN                                                       16 00660
              IF(DABS(DBLE(X)/DBLE(ESAV(1))-1.D+0) .LT. DBLE(EPREC))THEN16 00670
                 FNEW=FSAV(1)                                           16 00680
                 RETURN                                                 16 00690
              ENDIF                                                     16 00700
           ENDIF                                                        16 00710
C          Rewrote comparisons in double precision and tightened up     16 00720
C            the tests - picking wrong value sometimes (TWB. 930112)    16 00730
           IF(LOWER .NE. 0)THEN                                         16 00740
              IF(DABS(DBLE(X)-DBLE(ESAV(LOWER))) .LE.                   16 00750
     2          DABS(DBLE(X)-DBLE(ESAV(LOWER+1))))THEN                  16 00760
                 THPCK=LOWER                                            16 00770
              ELSE                                                      16 00780
                 THPCK=LOWER+1                                          16 00790
              ENDIF                                                     16 00800
              IF(DABS(DBLE(X)/DBLE(ESAV(THPCK))-1.D+0)                  16 00810
     2          .LT. DBLE(EPREC))THEN                                   16 00820
                 FNEW=FSAV(THPCK)                                       16 00830
                 RETURN                                                 16 00840
              ENDIF                                                     16 00850
              NSTEP=MIN0(N-1,NSTEPM)                                    16 00860
              IF(LOWER .GT. N-1-NSTEP)THEN                              16 00870
                 LOWER=N-1-NSTEP                                        16 00880
              ELSE                                                      16 00890
                 LOWER=LOWER-NSTEP/2                                    16 00900
              ENDIF                                                     16 00910
              IF(LOWER .GE. 1)THEN                                      16 00920
                 NSTEP=MIN0(N-1-LOWER,NSTEPM)                           16 00930
                 CALL POLINT(ESAV(LOWER),FSAV(LOWER),NSTEP,X,Y,DY)      16 00940
C                Changed check on precision to double precision -       16 00950
C                  convergence problems (TWB. 930212)                   16 00960
                 IF(Y .NE. 0.)THEN                                      16 00970
                    IF(DABS(DBLE(DY)/DBLE(Y)) .LE. EPREC)THEN           16 00980
                       FNEW=Y                                           16 00990
                       RETURN                                           16 01000
                    ENDIF                                               16 01010
                 ENDIF                                                  16 01020
              ENDIF                                                     16 01030
           ENDIF                                                        16 01040
        ENDIF                                                           16 01050
        FNEW=F(X)                                                       16 01060
      IF(N .GT. NTEST)THEN                                              16 01070
         IF(FSAV(2) .EQ. 0.)THEN                                        16 01080
            DO 300 I=(N-1)/2,2,-1                                       16 01090
               IF(FSAV(I) .EQ. 0. .AND. I .GT. 1)THEN                   16 01100
                  ISLOW=.TRUE.                                          16 01110
                  LOWZER=ESAV(I)                                        16 01120
                  GOTO 310                                              16 01130
               ENDIF                                                    16 01140
300         CONTINUE                                                    16 01150
310         CONTINUE                                                    16 01160
            JJ=1                                                        16 01170
            DO 320 J=I,N-1                                              16 01180
               ESAV(JJ)=ESAV(J)                                         16 01190
               FSAV(JJ)=FSAV(J)                                         16 01200
               JJ=JJ+1                                                  16 01210
320         CONTINUE                                                    16 01220
            N=JJ                                                        16 01230
         ENDIF                                                          16 01240
         IF(FSAV(N-2) .EQ. 0.)THEN                                      16 01250
            DO 400 I=(N-1)/2,N-2                                        16 01260
               IF(FSAV(I) .EQ. 0. .AND. I .LT. N-1)THEN                 16 01270
                  ISHIG=.TRUE.                                          16 01280
                  HGHZER=ESAV(I)                                        16 01290
                  GOTO 410                                              16 01300
               ENDIF                                                    16 01310
400         CONTINUE                                                    16 01320
410         CONTINUE                                                    16 01330
            N=I+1                                                       16 01340
         ENDIF                                                          16 01350
      ENDIF                                                             16 01360
      IF(N .GT. NSAV)RETURN                                             16 01370
        ESAV(N)=X                                                       16 01380
        FSAV(N)=FNEW                                                    16 01390
        IF(N .GT. 1)THEN                                                16 01400
           DO 200 I=1,N-1                                               16 01410
              DO 100 J=I+1,N                                            16 01420
                 IF(ESAV(I) .GT. ESAV(J))THEN                           16 01430
                    TEMP1=ESAV(J)                                       16 01440
                    TEMP2=FSAV(J)                                       16 01450
                    ESAV(J)=ESAV(I)                                     16 01460
                    FSAV(J)=FSAV(I)                                     16 01470
                    ESAV(I)=TEMP1                                       16 01480
                    FSAV(I)=TEMP2                                       16 01490
                 ENDIF                                                  16 01500
100           CONTINUE                                                  16 01510
200        CONTINUE                                                     16 01520
        ENDIF                                                           16 01530
        N=N+1                                                           16 01540
        RETURN                                                          16 01550
        END                                                             16 01560
        SUBROUTINE LOCATE(XX,N,X,J)                                     17 00010
C                                                                       17 00020
C  Given an array XX of length N, and a given value X, returns a value J17 00030
C    such that X is between XX(J) and XX(J+1).  XX must be monotonic,   17 00040
C    either increasing or decreasing.  J=0 or J=N is returned to indicat17 00050
C    that X is out of the range.                                        17 00060
C    [Numerical Recipes (The Art of Scientific Computing.  W.H. Press,  17 00070
C    et al.  Cambridge University Press (NY, 1986), p.90]               17 00080
C                                                                       17 00090
        INTEGER N,J                                                     17 00100
        REAL XX(N),X                                                    17 00110
C                                                                       17 00120
        INTEGER JL,JU,JM                                                17 00130
C                                                                       17 00140
        JL=0                                                            17 00150
        JU=N+1                                                          17 00160
10      IF(JU-JL .GT. 1)THEN                                            17 00170
           JM=(JU+JL)/2                                                 17 00180
           IF((XX(N) .GT. XX(1)) .EQV. (X .GT. XX(JM)))THEN             17 00190
              JL=JM                                                     17 00200
           ELSE                                                         17 00210
              JU=JM                                                     17 00220
           ENDIF                                                        17 00230
           GOTO 10                                                      17 00240
        ENDIF                                                           17 00250
        J=JL                                                            17 00260
        RETURN                                                          17 00270
        END                                                             17 00280
      SUBROUTINE ZBLANK(X,DX)                                           18 00010
C                                                                       18 00020
C   if x is zero then x and dx are set to blank                         18 00030
C                                                                       18 00040
      CHARACTER *(*) X,DX                                               18 00050
C                                                                       18 00060
      INTEGER I,J                                                       18 00070
C                                                                       18 00080
      INTEGER LENSTR                                                    18 00090
      EXTERNAL LENSTR                                                   18 00100
C                                                                       18 00110
      J=LENSTR(X)                                                       18 00120
      DO 100 I=1,J                                                      18 00130
         IF(X(I:I).NE.'.' .AND. X(I:I).NE.' ' .AND.                     18 00140
     1      X(I:I).NE.'0') RETURN                                       18 00150
  100 CONTINUE                                                          18 00160
      X=' '                                                             18 00170
      DX=' '                                                            18 00180
      RETURN                                                            18 00190
      END                                                               18 00200
      SUBROUTINE PROCE                                                  19 00010
C                                                                       19 00020
C   process E card                                                      19 00030
C                                                                       19 00040
      COMMON /CARDIM/CARD,CARD2,STARS                                   19 00050
      CHARACTER*80  CARD                                                19 00060
      CHARACTER*80  CARD2                                               19 00070
      CHARACTER* 8  STARS                                               19 00080
C                                                                       19 00090
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          19 00100
     1    IBONLY,ZERUNC,EK,NUMP                                         19 00110
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             19 00120
      REAL    DPERC, DPINT,DEINT                                        19 00130
      REAL    PERC, PINT, EINT                                          19 00140
      REAL    EK(5)                                                     19 00150
C                                                                       19 00160
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              19 00170
      CHARACTER* 2  TYPE                                                19 00180
      CHARACTER     ATI*10,  DATI*2                                     19 00190
      CHARACTER*2   UNCERT                                              19 00200
C                                                                       19 00210
      INTEGER J2PREC                                                    19 00220
      CHARACTER*10  STA                                                 19 00230
      CHARACTER* 2  STB                                                 19 00240
C                                                                       19 00250
      INTEGER IPREC                                                     19 00260
      EXTERNAL IPREC                                                    19 00270
      Save                                                              19 00280
C                                                                       19 00290
 9006 FORMAT(11X,A, /,'0')                                              19 00300
 9007 FORMAT(35X,A )                                                    19 00310
C                                                                       19 00320
         WRITE(36, FMT='(A)')                                           19 00330
         IZ = IABS(IZ)                                                  19 00340
         TYPE=CARD(78:79)                                               19 00350
C                                                                       19 00360
C     TI field (65-74)                                                  19 00370
C                                                                       19 00380
         STA=CARD(65:74)                                                19 00390
         IF(STA .NE. ' ') THEN                                          19 00400
C     TI VALUE GIVEN - Use TI for total intensity                       19 00410
             STB=CARD(75:76)                                            19 00420
C                                                                       19 00430
C-SAVE TOTAL INTENSITY FROM THE TI-FIELD.                               19 00440
C                                                                       19 00450
             ATI=STA                                                    19 00460
             DATI=STB                                                   19 00470
             UNCERT=STB                                                 19 00480
C                                                                       19 00490
C     SAVE PRECISION OF INPUT                                           19 00500
             JPREC = IPREC(STA)                                         19 00510
             CALL CNVS2U(STA, STB, PERC, DPERC)                         19 00520
C     NOTE IF DPERC = 0.0 (MISSING UNCERT)                              19 00530
C        Program was wrongly setting ZERUNC when pure 100% decay        19 00540
C          (TWB. 930212)                                                19 00550
         IF ((DPERC .EQ. 0.0 .AND. PERC .NE. 100.) .OR. STB .NE. ' ')   19 00560
     2     ZERUNC = 1                                                   19 00570
             EINT = PERC                                                19 00580
             DEINT = DPERC                                              19 00590
C                                                                       19 00600
C-REPLACE OLD IB, IE WITH BLANKS.                                       19 00610
C                                                                       19 00620
             CARD(22:41)=' '                                            19 00630
         ELSE                                                           19 00640
C                                                                       19 00650
C-NO ENTRY GIVEN IN THE TI-FIELD.                                       19 00660
C-DECODE POSITRON INTENSITY.(IB) (22-29)                                19 00670
C                                                                       19 00680
             STA=CARD(22:29)                                            19 00690
C     SAVE PRECISION OF INPUT                                           19 00700
             JPREC = IPREC(STA)                                         19 00710
             STB=CARD(30:31)                                            19 00720
             CALL CNVS2U(STA, STB, PINT, DPINT)                         19 00730
             UNCERT=STB                                                 19 00740
C                                                                       19 00750
C-DECODE CAPTURE INTENSITY.(IE) (32-39)                                 19 00760
C   (if both TI and IB are blank, then use IE's uncertainty for UNCERT  19 00770
C                                                                       19 00780
             STA=CARD(32:39)                                            19 00790
C     SAVE PRECISION OF INPUT                                           19 00800
             J2PREC = IPREC(STA)                                        19 00810
C     USE GREATER PRECISION                                             19 00820
             IF (J2PREC .GT. JPREC) JPREC = J2PREC                      19 00830
             STB=CARD(40:41)                                            19 00840
             CALL CNVS2U(STA, STB, EINT, DEINT)                         19 00850
C                                                                       19 00860
C   If IB uncertainty is numeric or IB's are not given then             19 00870
C   save STB for UNCERTainty                                            19 00880
C                                                                       19 00890
             IF(PINT.EQ.0. .AND. UNCERT.EQ.' ') UNCERT=STB              19 00900
C           Uncertainty from IE field should only be used if dominate   19 00910
C             (TWB. 930212)                                             19 00920
             IF(DEINT.GT.0 .AND. EINT .GT. PINT) UNCERT=STB             19 00930
C                                                                       19 00940
C-FIND TOTAL INTENSITY.                                                 19 00950
C                                                                       19 00960
             PERC = PINT + EINT                                         19 00970
             DPERC = SQRT(DPINT * DPINT + DEINT * DEINT)                19 00980
C                                                                       19 00990
C   if both IB(PINT) and IE(EINT) are given, blank out IB, IE fields    19 01000
C                                                                       19 01010
             IF (PINT .NE. 0.0 .AND. EINT .NE. 0.0) THEN                19 01020
                 CALL CNVU2S(PERC, DPERC, ATI, 10, DATI, 2)             19 01030
                 CARD(22:41)=' '                                        19 01040
             ENDIF                                                      19 01050
C     NOTE IF DPINT OR DEINT = 0.0                                      19 01060
C     BUT THEIR CORRESP VALUES NOT = 0.0 and not equal to 100           19 01070
             ZERUNC = 0                                                 19 01080
C           Program was not correctly setting all ZERUNC=1 cases        19 01090
C             and not calculating uncertainty when 100% decay           19 01100
C             (TWB. 930112)                                             19 01110
             IF ((DPINT .EQ. 0.0 .AND. PINT .NE. 0.0                    19 01120
     2         .AND. PINT .NE. 100.)                                    19 01130
     3         .OR. (uncert(1:1) .GE. 'A' .AND. uncert(1:1) .LE. 'Z'))  19 01140
     4         ZERUNC = 1                                               19 01150
             IF ((DEINT .EQ. 0.0 .AND. EINT .NE. 0.0                    19 01160
     2         .AND. EINT .NE. 100.)                                    19 01170
     3         .OR. (uncert(1:1) .GE. 'A' .AND. uncert(1:1) .LE. 'Z'))  19 01180
     4         ZERUNC = 1                                               19 01190
             IF (EINT .EQ. 0.) THEN                                     19 01200
                 WRITE(36, 9006)'NO CAPTURE INTENSITY GIVEN.'           19 01210
                 IF(PINT.NE.0.) IBONLY = 1                              19 01220
             ENDIF                                                      19 01230
             IF (PINT .NE. 0.) THEN                                     19 01240
C                                                                       19 01250
C   IB (PINT) given                                                     19 01260
C                                                                       19 01270
                 IF (EINT.NE.0.) THEN                                   19 01280
                    IF ((DEINT .NE. 0.) .AND. (DPINT .NE. 0.)) THEN     19 01290
                       DPERC = PERC * AMIN1(DEINT / EINT, DPINT / PINT) 19 01300
                    ENDIF                                               19 01310
                 ENDIF                                                  19 01320
                 RETURN                                                 19 01330
             ELSE                                                       19 01340
C                                                                       19 01350
C-IF NO POSITRON INTENSITY, THEN ASSUME IE IS TOTAL.                    19 01360
C-SAVE TOTAL INTENSITY IN THE TI-FIELD.                                 19 01370
C                                                                       19 01380
                ATI=STA                                                 19 01390
                DATI=STB                                                19 01400
                WRITE(36,9007)                                          19 01410
     1              'IE is assumed to be the total intensity'           19 01420
             ENDIF                                                      19 01430
         ENDIF                                                          19 01440
C  use last EK if there are more                                        19 01450
C        Program was outputing extraneous messages (TWB. 930212)        19 01460
         IF (EK(NUMP) .GT. 1022. .AND. pint .EQ. 0.) WRITE(36, 9007)    19 01470
     1      'NO POSITRON INTENSITY GIVEN'                               19 01480
      RETURN                                                            19 01490
      END                                                               19 01500
      SUBROUTINE WECARD                                                 20 00010
C                                                                       20 00020
C   Place IE, IB in proper fields on the new first E-Card               20 00030
C                                                                       20 00040
      COMMON /CARDIM/CARD,CARD2,STARS                                   20 00050
      CHARACTER*80  CARD                                                20 00060
      CHARACTER*80  CARD2                                               20 00070
      CHARACTER* 8  STARS                                               20 00080
C                                                                       20 00090
      COMMON /ENUMS/IZ,JPREC,PERC,DPERC,PINT,DPINT,EINT,DEINT,          20 00100
     1    IBONLY,ZERUNC,EK,NUMP                                         20 00110
      INTEGER IZ, JPREC,IBONLY ,ZERUNC,NUMP                             20 00120
      REAL    DPERC, DPINT,DEINT                                        20 00130
      REAL    PERC, PINT, EINT                                          20 00140
      REAL    EK(5)                                                     20 00150
C                                                                       20 00160
      COMMON /ECHARS/ TYPE,ATI,DATI,UNCERT                              20 00170
      CHARACTER* 2  TYPE                                                20 00180
      CHARACTER     ATI*10,  DATI*2                                     20 00190
      CHARACTER*2   UNCERT                                              20 00200
C                                                                       20 00210
      COMMON /WENUM/ AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM,     20 00220
     1    FCLP,FCMNM, FCMNO, FCMNP,NPOS,ETOP                            20 00230
      INTEGER NPOS                                                      20 00240
      REAL   AREA, AREAM, AREAP,FCK, FCKM, FCKP, FCL, FCLM, FCLP,       20 00250
     1    FCMNM, FCMNO, FCMNP,ETOP                                      20 00260
C                                                                       20 00270
      INTEGER LPREC,DELTMP,theprc                                       20 00280
      REAL EKTOT,DEKTOT,ELTOT,DELTOT,EMTOT,DEMTOT                       20 00290
      REAL EKTOTM,EKTOTP,ELTOTM,ELTOTP,EMTOTM,EMTOTP                    20 00300
      REAL DPREC                                                        20 00310
      CHARACTER*2  STB                                                  20 00320
      Character seint*8,dseint*2                                        20 00330
      Character spint*8,dspint*2                                        20 00340
      Character*11 scard2                                               20 00350
C                                                                       20 00360
      INTEGER INT,LEN,NINT                                              20 00370
      REAL ALOG10                                                       20 00380
      INTRINSIC ALOG10,INT,LEN,NINT                                     20 00390
C                                                                       20 00400
      INTEGER LENSTR                                                    20 00410
      EXTERNAL LENSTR                                                   20 00420
      Save                                                              20 00430
C                                                                       20 00440
         EKTOT = FCK / AREA                                             20 00450
         EKTOTP = FCKP / AREAP                                          20 00460
         EKTOTM = EKTOT                                                 20 00470
         IF (AREAM .GT. 0.) EKTOTM = FCKM / AREAM                       20 00480
         DEKTOT = AMAX1(ABS(EKTOTP - EKTOT), ABS(EKTOT - EKTOTM))       20 00490
         ELTOT = FCL / AREA                                             20 00500
         ELTOTP = FCLP / AREAP                                          20 00510
         ELTOTM = ELTOT                                                 20 00520
         IF (AREAM .GT. 0.) ELTOTM = FCLM / AREAM                       20 00530
         DELTOT = AMAX1(ABS(ELTOT - ELTOTP), ABS(ELTOT - ELTOTM))       20 00540
         EMTOT = FCMNO / AREA                                           20 00550
         EMTOTP = FCMNP / AREAP                                         20 00560
         EMTOTM = EMTOT                                                 20 00570
         IF (AREAM .GT. 0.) EMTOTM = FCMNM / AREAM                      20 00580
         DEMTOT = AMAX1(ABS(EMTOT - EMTOTP), ABS(EMTOT - EMTOTM))       20 00590
         WRITE(36, 9019) EKTOT, DEKTOT, ELTOT, DELTOT, EMTOT, DEMTOT    20 00600
 9019 FORMAT(20X, 'K/(EC+B+)=', 1PE10.4, '+-', E9.2,                    20 00610
     1    5X, 'L/(EC+B+)=', 1PE10.4, '+-', E9.2,                        20 00620
     2    5X, 'MNO/(EC+B+)=', 1PE10.4, '+-', E9.2)                      20 00630
C                                                                       20 00640
C     NPOS=0    EC only decay                                           20 00650
C                                                                       20 00660
         Call Lbsup(ati)                                                20 00670
         IF (NPOS .EQ. 0) THEN                                          20 00680
            If(Lenstr(ati) .GT. LEN(seint))Write(36,fmt='(3A)')         20 00690
     2        ' Warning: ',ati(1:Lenstr(ati)),' will be truncated'      20 00700
            seint=ATI                                                   20 00710
            dseint=DATI                                                 20 00720
            spint=STARS                                                 20 00730
            GO TO 510                                                   20 00740
         ENDIF                                                          20 00750
C        Reduced limit to output IB and IE from 0.01% to 0.001%         20 00760
C          (TWB. 930212)                                                20 00770
         IF ((EINT .LT. 0.001) .AND. (ETOP .LT. 0.001)) THEN            20 00780
            If(Lenstr(ati) .GT. LEN(spint))Write(36,fmt='(3A)')         20 00790
     2        ' Warning: ',ati(1:Lenstr(ati)),' will be truncated'      20 00800
             spint=ATI                                                  20 00810
             dspint=DATI                                                20 00820
             seint=STARS                                                20 00830
             GOTO 510                                                   20 00840
         ENDIF                                                          20 00850
         IF ((PINT .LT. 0.001) .AND. (ETOP .GT. 1000.)) THEN            20 00860
            seint=ATI                                                   20 00870
            dseint=DATI                                                 20 00880
            spint=STARS                                                 20 00890
            GO TO 510                                                   20 00900
         ENDIF                                                          20 00910
C                 TO PREVENT WRITING VALUES WHOSE UNCERTAINTY IS        20 00920
C                 LARGER THAN THE VALUE BY A FACTOR OF 5                20 00930
         IF (DEINT / EINT .GT. 5.0) ZERUNC = 1                          20 00940
         IF (DPINT / PINT .GT. 5.0) ZERUNC = 1                          20 00950
         IF (ZERUNC .EQ. 0) THEN                                        20 00960
            CALL CNVU2S(EINT, DEINT, seint, 8, dseint, 2)               20 00970
            CALL CNVU2S(PINT, DPINT, spint, 8, dspint, 2)               20 00980
         ELSE                                                           20 00990
C                 PRINT USING CALCULATED PRECISION                      20 01000
C           Logic was mixed up. Wrong value of JPREC was being used and 20 01010
C             wrong uncertainty was being output for the smaller of     20 01020
C             EINT and PINT (TWB. 921216)                               20 01030
C           Continued refinement of logic (TWB. 930212)                 20 01040
            If(eint .GT. 1.)Then                                        20 01050
               lprec=INT(ALOG10(EINT))+1                                20 01060
            Else                                                        20 01070
               LPREC=INT(ALOG10(EINT))                                  20 01080
            Endif                                                       20 01090
            theprc=lprec-jprec                                          20 01100
            DPREC=3.0*10.0**theprc                                      20 01110
            DELTMP=NINT(DEINT/10.0**theprc)                             20 01120
1000        CONTINUE                                                    20 01130
            IF(DELTMP .GT. 25)THEN                                      20 01140
               theprc=theprc+1                                          20 01150
               DPREC=DPREC*10.                                          20 01160
               DELTMP=NINT(DEINT/10.0**theprc)                          20 01170
               GoTo 1000                                                20 01180
            ENDIF                                                       20 01190
            CALL CNVU2S(EINT, DPREC, seint, 8, dseint, 2)               20 01200
            CALL KNVI2S(DELTMP,dseint,0)                                20 01210
            If(pint .GT. 1.)Then                                        20 01220
               lprec=INT(ALOG10(PINT))+1                                20 01230
            Else                                                        20 01240
               LPREC=INT(ALOG10(PINT))                                  20 01250
            Endif                                                       20 01260
            theprc=lprec-jprec                                          20 01270
            DPREC=3.0*10.0**theprc                                      20 01280
            DELTMP=NINT(DPINT/10.0**theprc)                             20 01290
1100        CONTINUE                                                    20 01300
            IF(DELTMP .GT. 25)THEN                                      20 01310
               theprc=theprc+1                                          20 01320
               DPREC=DPREC*10.                                          20 01330
               DELTMP=NINT(DPINT/10.0**theprc)                          20 01340
               GoTo 1100                                                20 01350
            ENDIF                                                       20 01360
            CALL CNVU2S(PINT, DPREC, spint, 8, dspint, 2)               20 01370
            CALL KNVI2S(DELTMP,dspint,0)                                20 01380
            IF(LENSTR(UNCERT) .EQ. 0)THEN                               20 01390
               dseint = ' '                                             20 01400
               dspint = ' '                                             20 01410
            ELSE                                                        20 01420
               IF(UNCERT(1:1) .GE. 'A' .AND. UNCERT(1:1) .LE. 'Z')THEN  20 01430
                  dseint=UNCERT                                         20 01440
                  dspint=UNCERT                                         20 01450
               ENDIF                                                    20 01460
            ENDIF                                                       20 01470
         ENDIF                                                          20 01480
  510    CONTINUE                                                       20 01490
C                                                                       20 01500
C   if the number is all 0, then blank out                              20 01510
C                                                                       20 01520
         CALL ZBLANK(spint,dspint)                                      20 01530
         CALL ZBLANK(seint,dseint)                                      20 01540
         IF (seint(1:8) .NE. STARS) THEN                                20 01550
             CALL LBSUP(seint)                                          20 01560
             CARD(32:39)=seint                                          20 01570
             CARD(40:41)=dseint                                         20 01580
         ENDIF                                                          20 01590
         IF (spint(1:8) .NE. STARS) THEN                                20 01600
             CALL LBSUP(spint)                                          20 01610
             CARD(22:29)=spint                                          20 01620
             CARD(30:31)=dspint                                         20 01630
         ENDIF                                                          20 01640
         CALL CENTER(CARD(22:29),8)                                     20 01650
         CALL CENTER(CARD(32:39),8)                                     20 01660
C                                                                       20 01670
C-PREPARE NEW SECOND E-CARD.                                            20 01680
C                                                                       20 01690
C     Do not output capture fractions if IE is not output (TWB 930212)  20 01700
      If(card(32:39) .NE. ' ')Then                                      20 01710
         If(ektot .GT. 0.)Call Addfrac(card2,ektot,dektot,'CK')         20 01720
         If(eltot .GT. 0.)Call Addfrac(card2,eltot,deltot,'CL')         20 01730
         If(emtot .GT. 0.)Call Addfrac(card2,emtot,demtot,'CM+')        20 01740
      Endif                                                             20 01750
      RETURN                                                            20 01760
      END                                                               20 01770
      Subroutine Addfrac(card,frac,dfrac,text)                          21 00010
C     Adds electron-capture fractions to the new "S E" record           21 00020
C        card   "S E" record                                            21 00030
C        frac   capture fraction                                        21 00040
C        dfrac  uncertainty in capture fraction                         21 00050
C        text   type of fraction                                        21 00060
C                                                                       21 00070
      Character*(*)card                                                 21 00080
      Real frac,dfrac                                                   21 00090
      Character*(*) text                                                21 00100
C                                                                       21 00110
      Integer lprec,start                                               21 00120
      Character*2  dsx                                                  21 00130
      Character*11 sx                                                   21 00140
C                                                                       21 00150
      Integer Lenstr                                                    21 00160
      External Lenstr                                                   21 00170
C                                                                       21 00180
      Integer INDEX,NINT                                                21 00190
      Real ALOG10                                                       21 00200
      Intrinsic ALOG10,INDEX,NINT                                       21 00210
C                                                                       21 00220
      If(Lenstr(card) .LT. 10)Then                                      21 00230
         start=10                                                       21 00240
      Else                                                              21 00250
         start=Lenstr(card)+1                                           21 00260
      EndIf                                                             21 00270
C                                                                       21 00280
      If(dfrac/frac .LE. 0.0001)Then                                    21 00290
         dfrac=0.001*frac                                               21 00300
         Call Cnvu2s(frac,0.0,sx,11,dsx,0)                              21 00310
      ElseIf(dfrac/frac .LE. 0.001)Then                                 21 00320
         Call Cnvu2s(frac,dfrac,sx,11,dsx,-1)                           21 00330
      Else                                                              21 00340
         Call Cnvu2s(frac,dfrac,sx,11,dsx,2)                            21 00350
         Call Lbsup(sx)                                                 21 00360
         Call Lbsup(dsx)                                                21 00370
         Call Addstr(sx,Lenstr(sx)+2,dsx)                               21 00380
      EndIf                                                             21 00390
      If(INDEX(sx,'*') .GT. 0)Then                                      21 00400
         lprec=NINT(ALOG10(frac)-ALOG10(dfrac))                         21 00410
         Call Cnvu2s(frac,0.0,sx,11,dsx,-lprec)                         21 00420
      Endif                                                             21 00430
      If(INDEX(sx,'*') .EQ. 0)Then                                      21 00440
         card(start:)=text                                              21 00450
         start=start+Lenstr(text)                                       21 00460
         Call Addstr(card,start,'=')                                    21 00470
         Call Lbsup(sx)                                                 21 00480
         start=start+1                                                  21 00490
         Call Addstr(card,start,sx)                                     21 00500
         If(text .NE. 'CM+')Then                                        21 00510
            start=start+Lenstr(sx)                                      21 00520
            Call Addstr(card,start,'$')                                 21 00530
         EndIf                                                          21 00540
      EndIf                                                             21 00550
C                                                                       21 00560
      Return                                                            21 00570
      End                                                               21 00580
C+++MDC+++                                                              22 00010
C...VAX, DVF, UNX                                                       22 00020
      SUBROUTINE DATE_20(DATE)                                          22 00030
                                                                        22 00040
      CHARACTER DATE*(*)                                                22 00050
C                                                                       22 00060
C     RETURNS DATE AS A CHARACTER STRING OF 11 CHARACTERS IN THE        22 00070
C          FORM  DD-MMM-YYYY                                            22 00080
C                                                                       22 00090
      Character*3 mon(12)                                               22 00100
      Data mon/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',   22 00110
     +'Oct','Nov','Dec'/                                                22 00120
C...VAX, DVF                                                            22 00130
      Integer im                                                        22 00140
      Character*4 ccyy                                                  22 00150
      Character*2 dd                                                    22 00160
      CHARACTER DAY*8                                                   22 00170
C                                                                       22 00180
C     GET THE DATE AS A CHARACTER STRING CCYYMMDD                       22 00190
C                                                                       22 00200
      CALL DATE_AND_TIME(DAY)                                           22 00210
      Read(day,'(A4,I2,A2)') ccyy,im,dd                                 22 00220
      WRITE(DATE,FMT='(A2,''-'',A3,''-'',A4)') dd,MON(im),ccyy          22 00230
C...UNX                                                                 22 00240
C/      INTEGER IYMD(3)                                                 22 00250
C/      CALL IDATE(IYMD)                                                22 00260
C/      WRITE(DATE,FMT='(I2,''-'',A3,''-'',I4)') IYMD(1),MON(IYMD(2)),  22 00270
C/     +   IYMD(3)                                                      22 00280
C...VAX, DVF, UNX                                                       22 00290
      RETURN                                                            22 00300
      END                                                               22 00310
C---MDC---                                                              23 00010

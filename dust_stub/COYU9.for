      PROGRAM COYU9
      INTEGER ERRLEV
      CALL UNSL9(ERRLEV)
      IF (ERRLEV.EQ.0) CALL UNIF9(ERRLEV)
C      STOP
      END

      SUBROUTINE UNSL9(ERRLEV)
C----------------------------------------------------
C
C        PROGRAM TO RETRIEVE DATA FROM M AND J FILES
C          FOR UNIFORMITY ANALYSIS USING UNIF3
C
C                    21 JUNE 1994
C
C           CONVERTED TO FORTRAN 90 12/12/96
C
C        MODIFIED 190209 BY SALLY WATSON TO CREATE NEW COYUOP 
C        FILES AND NOT OVERWRITE OLD ONES
C
C-----------------------------------------------------
      CHARACTER*8 TITLE
      CHARACTER*80 UFILE,COYUOP
      CHARACTER*1 DU
      INTEGER ERRLEV
      REAL,DIMENSION(:),ALLOCATABLE::Z,SZ
      REAL,DIMENSION(:,:),ALLOCATABLE::A
      INTEGER,DIMENSION(:),ALLOCATABLE::NUMD,NCDE,NCN,IVS,MVARS,NCHR,NY,
     1NV,NUMC,SDCHAR,MNCHAR,SDVAR,MNVAR
      CHARACTER(LEN=8),DIMENSION(:),ALLOCATABLE::CHR,CNAME,DNAME
      CHARACTER(LEN=12),DIMENSION(:),ALLOCATABLE::VRY
      CHARACTER(LEN=12),DIMENSION(:,:),ALLOCATABLE::VNAME
      CHARACTER(LEN=80),DIMENSION(:),ALLOCATABLE::MFILE,JFILE
      CHARACTER(LEN=8),DIMENSION(:,:),ALLOCATABLE::HEAD
      DIMENSION TITLE(9)
      LOGICAL CHRABS

      ERRLEV=0
      OPEN(7,FILE='COYU9.DAT',STATUS='OLD')
      READ(7,952)UFILE
      READ(7,952)COYUOP
      CLOSE(1)
      OPEN(1,FILE=UFILE,STATUS='OLD')
      READ(1,1)NCP,NS,LY,NTR,TITLE
    1 FORMAT(I2,I1,I2,I2,1X,9A8)
      READ(1,*)NYEARS,NVARS,NCHS
      N9=9
      ALLOCATE(MVARS(NVARS),NCHR(NCHS),MFILE(NYEARS),JFILE(NYEARS),
     1HEAD(N9,NYEARS),NY(NYEARS),NV(NYEARS),VRY(NVARS),Z(NYEARS),
     2SZ(NYEARS),CHR(NCHS),IVS(NVARS))
      READ(1,*)(MVARS(I),I=1,NVARS)
      READ(1,*)(NCHR(I),I=1,NCHS)
      DO 951 I=1 ,NYEARS
      READ(1,952)MFILE(I)
  952 FORMAT(A80)
  951 CONTINUE
      DO 301 I=1,NYEARS
      READ(1,952)JFILE(I)
301   CONTINUE
      READ(1,*)NCAND
      ALLOCATE(NCN(NCAND))
      IF (NCAND.GT.0) READ(1,*)(NCN(I),I=1,NCAND)
      L=0
      DO 50 I=1,NVARS
      K=0
      DO 52 J=1,NCAND
      IF(MVARS(I).NE.NCN(J)) GOTO 52
      K=1
   52 CONTINUE
      IF(K.EQ.0) THEN
      L=L+1
      IVS(L)=I
      END IF
   50 CONTINUE
      NVS=L
      IF(L.EQ.NVARS-NCAND) THEN
        GOTO 53
      ELSE
        ERRLEV=1
        WRITE(6,54)
        GOTO 100
   54 FORMAT(//10X,'CANDIDATE WRONGLY SPECIFIED')
      END IF
   53 CONTINUE
      OPEN(2,FORM='FORMATTED',ACCESS='DIRECT',RECL=10,STATUS='SCRATCH')
      OPEN(4,FORM='FORMATTED',ACCESS='DIRECT',RECL=10,STATUS='SCRATCH')
      OPEN(3,FILE='U.DAT',STATUS='UNKNOWN')
      WRITE(3,'(A80)') COYUOP
      MAXV=-100
      MAXC=-100
      DO 310 K=1,NYEARS
      K1=8
      OPEN(K1,FILE=MFILE(K),STATUS='OLD')
      READ(K1,901)NCPK
      READ(K1,*)LV,LC
      IF(LV.GT.MAXV) THEN
          MAXV=LV
      END IF
      IF(LC.GT.MAXC) THEN
          MAXC=LC
      END IF
      CLOSE(K1)
310   CONTINUE
      ALLOCATE(NCDE(MAXV),VNAME(MAXV,NYEARS),NUMD(MAXC),DNAME(MAXC),
     1A(MAXV,MAXC),NUMC(MAXC),CNAME(MAXC))
      ALLOCATE(MNCHAR(NCHS),SDCHAR(NCHS),MNVAR(NVARS),SDVAR(NVARS))
      DO 821 JJ=1,NVARS
             MNVAR(JJ)=0
             SDVAR(JJ)=0
821   CONTINUE
      DO 822 JJ=1,NCHS
             MNCHAR(JJ)=0
             SDCHAR(JJ)=0
822   CONTINUE

      DO 5 K=1,NYEARS
      K1=8
      OPEN(K1,FILE=MFILE(K),STATUS='OLD')
      READ(K1,901)NCPK,NSK,NYRK,NTRK,DU,(HEAD(I,K),I=1,9)
  901 FORMAT(I2,I1,I2,I2,A1,9A8)
      NY(K)=NYRK
      IF(NCPK.NE.NCP) THEN
          ERRLEV=2
          WRITE(6,7)NCPK,MFILE(K)
          GO TO 100
    7     FORMAT(//5X,'CROP NUMBER',I4,' DOES NOT AGREE IN ',A80)
      END IF
      IF(NTRK.NE.NTR) THEN
           ERRLEV=3
           WRITE(6,302)NTRK,MFILE(K)
302        FORMAT(//5X,'TRIAL NUMBER',I4,' DOES NOT AGREE IN ',A80)
           GOTO 100
      END IF
      IF(NSK.NE.NS) THEN
           ERRLEV=4
           WRITE(6,303)NSK,MFILE(K)
303        FORMAT(//5X,'SITE NUMBER',I4,' DOES NOT AGREE IN ',A80)
           GOTO 100
      END IF
      READ(K1,*)NV(K),NCK,NRH
      NVK=NV(K)
      READ(K1,818)(NCDE(I),VNAME(I,K),I=1,NVK)
      READ(K1,18)(NUMD(I),DNAME(I),I=1,NCK)
  818 FORMAT(4X,I6,A12)
   18 FORMAT(I4,A8)
      DO 13 I=1,NVK
      READ(K1,*)(A(I,J),J=1,NCK)
13    CONTINUE
      DO 317 L=1,NCHS
      I2=0
      CHRABS=.FALSE.
      DO 17 M=1,NCK
      IF(NCHR(L).EQ.NUMD(M)) THEN
         MNCHAR(L)=MNCHAR(L)+1
         GO TO 19
      ENDIF
      I2=I2+1
      IF(I2.EQ.NCK) THEN
C ** Character is listed in UX file but not present in the current M file **
        CHRABS=.TRUE.
        GOTO 21
        ENDIF

      GO TO 17
   19 CONTINUE
      CHR(L)=DNAME(M)
   21 DO 315 I=1,NVARS
      I1=0
      DO 15 J=1,NVK
      IF(MVARS(I).EQ.NCDE(J)) THEN
        MNVAR(I)=MNVAR(I)+1
        GO TO 16
      ENDIF
      I1=I1+1
      IF(I1.EQ.NVK) THEN
        NRC=NVARS*NYEARS*(L-1)+NYEARS*(I-1)+K
        WRITE(2,'(F10.4)',REC=NRC) -1.00
        GOTO 15
      ENDIF

      GO TO 15
   16 CONTINUE
      VRY(I)=VNAME(J,K)
      NRC=NVARS*NYEARS*(L-1)+NYEARS*(I-1)+K
      IF (CHRABS) THEN
         WRITE(2,'(F10.4)',REC=NRC) -1.00
      ELSE
         WRITE(2,'(F10.4)',REC=NRC) A(J,M)
      ENDIF

   15 CONTINUE
315   CONTINUE
   17 CONTINUE
317   CONTINUE
      K2=9
      CLOSE(K2)
      OPEN(K2,FILE=JFILE(K),STATUS='OLD')
      READ(K2,901)NCPK,NSK,NYRK,NTRK,DU,(HEAD(I,K),I=1,9)
      IF(NCPK.NE.NCP) THEN
          ERRLEV=5
          WRITE(6,7)NCPK,JFILE(K)
          GO TO 100
      END IF
      IF(NTRK.NE.NTR) THEN
           ERRLEV=6
           WRITE(6,302)NTRK,JFILE(K)
           GOTO 100
      END IF
      IF(NSK.NE.NS) THEN
           ERRLEV=7
           WRITE(6,303)NSK,JFILE(K)
           GOTO 100
      END IF
      IF(NYRK.NE.NY(K)) THEN
           ERRLEV=8
           WRITE(6,307)NYRK,JFILE(K)
307        FORMAT(//5X,'YEAR NUMBER',I4,' DOES NOT AGREE IN ',A80)
           GOTO 100
      END IF
      READ(K2,*)N1,N2,N3
      IF (N1.NE.NVK) THEN
           ERRLEV=13
           WRITE(6,308) MFILE(K)
308        FORMAT(//5X,'MISMATCH IN NUMBER OF VARIETIES BETWEEN RELATED
     1 J FILE & ',/,5X,A80)
      GOTO 100
      ENDIF
      IF (N2.NE.NCK) THEN
           ERRLEV=14
           WRITE(6,309) MFILE(K)
309        FORMAT(//5X,'MISMATCH IN NUMBER OF CHARACTERS BETWEEN RELATED
     1 J FILE & ',/,5X,A80)
      GOTO 100
      ENDIF

      READ(K2,818)(NCDE(I),VNAME(I,K),I=1,N1)
      READ(K2,18)(NUMC(J),CNAME(J),J=1,N2)
      DO 212 I=1,N1
      READ(K2,*)(A(I,J),J=1,N2)
212   CONTINUE
      DO 387 L=1,NCHS
      I2=0
      CHRABS=.FALSE.
      DO 87 M=1,N2
      IF(NCHR(L).EQ.NUMC(M)) THEN
        SDCHAR(L)=SDCHAR(L)+1
        GO TO 89
      ENDIF
      I2=I2+1
      IF(I2.EQ.N2) THEN
C * Character is listed in the UX file but not present in the current J file *
        CHRABS=.TRUE.
        GOTO 88
        ENDIF

      GO TO 87
   89 CONTINUE
   88 DO 385 I=1,NVARS
      I1=0
      DO 85 J=1,N1
      IF(MVARS(I).EQ.NCDE(J)) THEN
        SDVAR(I)=SDVAR(I)+1
        GO TO 86
      ENDIF
      I1=I1+1
      IF(I1.EQ.N1) THEN
C * Variety missing from a J file but AFP in UX file *
        NRC=NVARS*NYEARS*(L-1)+NYEARS*(I-1)+K
        WRITE(4,'(F10.4)',REC=NRC) -1.00
        GOTO 85
        ENDIF

      GO TO 85
   86 CONTINUE
      NRC=NVARS*NYEARS*(L-1)+NYEARS*(I-1)+K
      IF (CHRABS) THEN
        WRITE(4,'(F10.4)',REC=NRC) -1.00
      ELSE
        WRITE(4,'(F10.4)',REC=NRC) A(J,M)
      ENDIF
   85 CONTINUE
385   CONTINUE
   87 CONTINUE
387   CONTINUE
      CLOSE(K1)
      CLOSE(K2)
    5 CONTINUE
      READ(7,*)NP,IPRINT,NPR
C       ++++++ CODE TO REPLACE LINES AT BOTTOM OF PROGRAM UNSL
C       ++++++ FROM STATEMENT NUMBER 752 TO STATEMENT NUMBER 101
      WRITE(3,70)NCP,NS,LY,NTR,(TITLE(I),I=1,9)
70    FORMAT(I2,I1,I2,I2,1X,9A8)
      WRITE(3,71)NVARS,NCHS,NYEARS,NVS,IPRINT,NPR
71    FORMAT(15I4)
C----------------------------------------
C        NPR CONTROLS OUTPUT WIDTH IN UNIF3
C             0 FOR 80 COLUMN O/P
C             1 FOR 120 COLUMN O/P
C------------------------------------------
      WRITE(3,778)NP
  778 FORMAT(I4)
      DO 777 I=1,NP
      READ(7,*)CRIT3,CRIT2,CRIT2A
      WRITE(3,78)CRIT3,CRIT2,CRIT2A
  777 CONTINUE
78    FORMAT(3F9.5)
      WRITE(3,71)(NY(I),I=1,NYEARS)
      WRITE(3,71)(IVS(I),I=1,NVS)
      WRITE(3,73)(I,MVARS(I),VRY(I),I=1,NVARS)
73    FORMAT(I4,I6,A12)
      NRC=0
      DO 77 II=1,NCHS
      WRITE(3,74) NCHR(II),CHR(II)
74    FORMAT(I3,1X,A8)
      DO 76 I=1,NVARS
      DO 46 L=1,NYEARS
      NRC=NRC+1
      READ(2,'(F10.4)',REC=NRC) Z(L)
      READ(4,'(F10.4)',REC=NRC) SZ(L)
   46 CONTINUE
      WRITE(3,75)MVARS(I),(Z(L),L=1,NYEARS),(SZ(L),L=1,NYEARS)
75    FORMAT(I6,8F9.3,3(/,4X,8F9.3))
76    CONTINUE
77    CONTINUE

      DO 823 JJ=1,NVARS
         IF (MNVAR(JJ).EQ.0) THEN
            WRITE(6,61) MVARS(JJ)
            ERRLEV=9
            GOTO 100
         ENDIF
         IF (SDVAR(JJ).EQ.0) THEN
            WRITE(6,64) MVARS(JJ)
            ERRLEV=12
            GOTO 100
         ENDIF
  823 CONTINUE

      DO 824 JJ=1,NCHS
         IF (MNCHAR(JJ).EQ.0) THEN
            WRITE(6,62) NCHR(JJ)
            ERRLEV=10
            GOTO 100
         ENDIF
         IF (SDCHAR(JJ).EQ.0) THEN
            WRITE(6,63) NCHR(JJ)
            ERRLEV=11
            GOTO 100
         ENDIF
  824 CONTINUE

  100 CONTINUE

      CLOSE(1)
      CLOSE(3)
      CLOSE(7)
   61 FORMAT(//10X,'VARIETY AFP',I6,' CANNOT BE FOUND IN ANY M FILES')
   62 FORMAT(//10X,'CHARACTER',I4,' CANNOT BE FOUND IN ANY M FILES')
   64 FORMAT(//10X,'VARIETY AFP',I6,' CANNOT BE FOUND IN ANY J FILES')
   63 FORMAT(//10X,'CHARACTER',I4,' CANNOT BE FOUND IN ANY J FILES')
      OPEN(1,FILE='VBERRORS.DAT',STATUS='UNKNOWN')
      SELECT CASE (ERRLEV)
      CASE (0)
      WRITE(1,66)
66    FORMAT(1X,'FORTRAN COMPLETED OK')
      CASE (1)
        WRITE(1,54)
      CASE (2)
        WRITE(1,7)NCPK,MFILE(K)
      CASE (3)
        WRITE(1,302)NTRK,MFILE(K)
      CASE (4)
         WRITE(1,303)NSK,MFILE(K)
      CASE (5)
         WRITE(1,7)NCPK,JFILE(K)
      CASE (6)
         WRITE(1,302)NTRK,JFILE(K)
      CASE (7)
         WRITE(1,303)NSK,JFILE(K)
      CASE (8)
          WRITE(1,307)NYRK,JFILE(K)
      CASE (9)
          WRITE(1,61) MVARS(JJ)
      CASE (10)
          WRITE(1,62) NCHR(JJ)
      CASE (11)
          WRITE(1,63) NCHR(JJ)
      CASE (12)
          WRITE(1,64) MVARS(JJ)
      CASE(13)
          WRITE(1,308) MFILE(J)
      CASE(14)
          WRITE(1,309) MFILE(J)
      END SELECT
      CLOSE(1)
      END

      SUBROUTINE UNIF9(ERRLEV)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C       FORTRAN 90 PROGRAM TO CALCULATE UNIFORMITY CRITERION BASED ON
C       OVER-YEARS ANALYSIS OF STANDARD DEVIATIONS AFTER THESE HAVE
C       BEEN ADJUSTED FOR WITHIN-YEAR REGRESSION ON CHARACTER MEANS .
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C
C       INPUT IS FROM A FILE READ THROUGH STREAM IN (=7) AND OUTPUT
C       IS TO STREAM NOUT (=8) .
C
C       THE FORMAT OF DATA IN THE INPUT FILE IS AS FOLLOWS -
C
C
C          TITLE     = PRINTED AS HEADING TO OUTPUT        (A72)
C          NV        = NO. OF VARIETIES                         "
C          NM        = NO. OF CHARACTERS                   (FREE-FORMAT)
C          NY        = NO. OF YEARS OR TRIALS                   "
C          NVS       = NO. OF REFERENCE VARIETIES               "
C          IPRINT    = PLOTS OPTION                             "
C                        0 - ONLY SUMMARY TABLE
C                        1 - ALL TABLES (PLOTS 3 PER PAGE)
C                        2 - ALL TABLES (PLOTS 1 PER PAGE)
C                       -1 - ALL TABLES - NO PLOTS
C          NP        = NO. OF COMBINATIONS OF PROBABILITY LEVELS FOLLOWED
C                      BY THE COMBINATIONS OF PROBABILITY LEVELS IN THREE
C             CRIT3     = PROBABILITY FOR CALCULATING REJECTION IN 3-YEAR TEST
C             CRIT2     = PROBABILITY FOR CALCULATING REJECTION IN 2-YEAR TEST
C             CRIT2A    = PROBABILITY FOR CALCULATING ACCEPTANCE IN 2-YEAR TEST
C          IYR       = YEAR (OR TRIAL) NUMBERS             (FREE-FORMAT)
C          IVS       = ORDER POSITION IN VNAME OF THE NVS STANDARD VARIETIES
C                       ( IF NVS=0 THEN IVS IS OMITTED AND ALL VARIETIES
C                              ARE USED AS STANDARD VARIETIES)

C *** Refs to NY and NY-1 changed to 3 and 2 years respectively for Crits ***
C *** by IMN 25.3.96 ***

C *** TITLE changed from A40 to A72 by IMN 3.2.97 ***
C
C     FOR EACH VARIATE THERE FOLLOWS
C
C          MNAME     = VARIATE NAME                        (A40)
C
C       FOLLOWED BY THE VARIATE VALUES -
C
C                    ----- MEANS ------       --- PLOT SDS ------
C      VARIETY CODE  YEAR1 YEAR 2 YEAR 3 ...  YEAR 1 YEAR 2 YEAR 3 ..
C
C      WITH THE FORMAT  I4,8F9.3,3(/,4X,8F9.3)
C
C
C      NOTE : THE FORMAT OF INPUT TO THIS PROGRAM IS SIMILAR TO THAT FOR
C             A DUST PROGRAM 'E' FILE , BUT WITH THE ADDITION OF THE
C             VARIABLES NVS,IPRINT,CRIT3,CRIT2,CRIT2A,IYR,IVS AT START
C
C
C     EXAMPLE OF INPUT -
C
C     133      PRG (DIPLOID) EARLY N.IRELAND
C      10 2 3 6 1 0.001 0.010 0.010
C      1986 1987 1988
C      1 2 3 4 7 8
C        1A          2B          3C          4D          5E          6F
C        7G          8H          9J         10K
C       5SPRINGHT
C      1  31.0  31.1  33.1   5.50  5.00  4.86
C      2  26.5  31.6  36.2   4.57  5.53  4.85
C      3  29.4  28.0  34.3   5.58  5.21  5.68
C      .
C      .
C      10 25.5  -1.0  33.6   4.70 -1.00  5.51
C       8DATEOFEE
C      1  71.5  73.7  69.7   4.38  5.95  6.64
C      2  65.1  70.3  69.9   5.46  8.79  5.94
C      .
C      .
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      REAL, DIMENSION(:), ALLOCATABLE::CRIT3,CRIT2,CRIT2A,RM,S,AM,
     1 SD,YASG
      INTEGER, DIMENSION(:), ALLOCATABLE::IYR,IVS,IVN,IMN
      CHARACTER(LEN=12), DIMENSION(:), ALLOCATABLE::VNAME
      CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE::MNAME
      REAL, DIMENSION(:,:), ALLOCATABLE::YAV1
      INTEGER, DIMENSION(:,:), ALLOCATABLE::IOY1,IWY1
      INTEGER, DIMENSION(:), ALLOCATABLE::MISMATCH
      CHARACTER*72 TITLE
      CHARACTER*80 COYUOP
      CHARACTER*1 SPACE
      INTEGER P,ERRLEV,MISCNT

C MISCNT is count of no. of mismatches of AFP nos  *** IMN 5.12.97 ***
C MISMATCH is array containing mismatched AFP nos  *** IMN 5.12.97 ***

      MISCNT=0
      ERRLEV=0
C
C   ++++ Set program constants
C
C         IN = INPUT DEVICE NUMBER
C         NOUT = OUTPUT DEVICE NUMBER
C         RMV = VALUE GIVEN TO MISSING REAL OBSERVATIONS
C
      OPEN(7,FILE='U.DAT',STATUS='OLD')
      IN=7
      READ(IN,1001) COYUOP
1001  FORMAT(A80)

C *** THE NEXT 4 LINES ADDED BY SW 190209 ***
      OPEN(8,FILE=COYUOP,STATUS='NEW',ERR=810)
      GOTO 811
  810 OPEN(8,FILE=COYUOP,STATUS='UNKNOWN')
  811 CONTINUE

C *** THE NEXT 1 LINE REMOVED BY SW 190209 ***
C      OPEN(8,FILE=COYUOP,STATUS='UNKNOWN')

      NOUT=8
      RMV=-1.0
C
C    +++++ Read details of data from file
C
      READ(IN,1004) TITLE
1004  FORMAT(A72)

      READ(IN,*)NV,NM,NY,NVS,IPRINT,NPR
      READ(IN,*) NP
      ALLOCATE(CRIT3(NP),CRIT2(NP),CRIT2A(NP))
      READ(IN,*)(CRIT3(I),CRIT2(I),CRIT2A(I)
     1               ,I=1,NP)
      N=NV*NY
      N1=N*NM
      NY1=NY-1
      NCV=NM*NV
C
C     +++++ READ YEAR NAMES
C
      ALLOCATE(IYR(NY),IVS(NV))

      READ(IN,*)(IYR(K),K=1,NY)
C
C     +++++ Read position of standard varieties in list
C
      IF(NVS.GT.0) THEN
        READ(IN,*)(IVS(I),I=1,NVS)
      ENDIF
C
C    +++++  Clear data stores to missing
C
      ALLOCATE(RM(N1),S(N1))

      DO 10 K=1,N1
        RM(K)=RMV
        S(K)=RMV
10    CONTINUE
C
C     +++++ READ VARIETY NAMES
C
      ALLOCATE(IVN(NV),VNAME(NV))
      IF (NVS.GT.0) THEN
         READ(IN,1006) (IVN(I),VNAME(I),I=1,NV)
      ELSE
         READ(IN,7006) IVN(1),VNAME(1)
         IF (NV.GE.2) READ(IN,1006) (IVN(I),VNAME(I),I=2,NV)
      ENDIF
1006  FORMAT(4X,I6,A12)
7006  FORMAT(/,4X,I6,A12)

C
C     +++++  DO FOR EACH MEASURE
C
      ALLOCATE(IMN(NM),MNAME(NM),AM(NY),SD(NY),MISMATCH(NCV))

      DO 900 M=1,NM
C
C     +++++  READ DATA
C
      IF ((M.GT.1).AND.(MOD(NY,4).EQ.0)) READ(IN,'(A1)') SPACE
      READ(IN,1007) IMN(M),MNAME(M)
1007  FORMAT(I3,1X,A8)
      DO 100 I=1,NV
        READ(IN,*) JJ,(AM(K),K=1,NY),(SD(K),K=1,NY)
C       WRITE(NOUT,1020)JJ,(AM(K),K=1,NY),(SD(K),K=1,NY)
C1020   FORMAT(I6,8F9.3)
        DO 80 K=1,NV
          IF(IVN(K).EQ.JJ) THEN
            J=K
            GOTO 85
          ENDIF
80      CONTINUE
        WRITE(NOUT,1010)JJ
        ERRLEV=1
        MISCNT=MISCNT+1
        MISMATCH(MISCNT)=JJ
        GOTO 100
85      DO 90 K=1,NY
          JK=(M-1)*NY*NV+(K-1)*NV+I
          RM(JK)=AM(K)
          S(JK)=SD(K)
90      CONTINUE
100   CONTINUE
900   CONTINUE
C
C     +++++ DO UNIFORMITY ANALYSIS FOR ALL NM MEASURES
C
      ALLOCATE(YAV1(NM,NV),IOY1(NM,NV),IWY1(NM,NV),YASG(NM))

      DO 150 P=1,NP
       CALL UNIF3(TITLE,NM,IMN,MNAME,NV,VNAME,NY,IYR,NVS,IVS,RM,S,NOUT,
     1            RMV,IPRINT,CRIT3(P),CRIT2(P),CRIT2A(P),
     3     YAV1,YASG,YCRI3,YCRI2,YCRI2A,IOY1,IWY1,N1,ERRLEV,NPR,IVN)
150   CONTINUE
      GOTO 999
1010  FORMAT(' VARIETY CODE',I4,' NOT RECOGNISED')
999   CONTINUE
      CLOSE(IN)
      CLOSE(NOUT)

      OPEN(1,FILE='VBERRORS.DAT',STATUS='UNKNOWN')
      SELECT CASE(ERRLEV)
      CASE(0)
      WRITE(1,66)
66    FORMAT(1X,'FORTRAN COMPLETED OK')
      CASE(1)
      WRITE(1,1010) (MISMATCH(II), II=1,MISCNT)
      CASE(2)
      WRITE(1,67)
67    FORMAT(1X,'PLEASE CHECK LIST OF REFERENCE VARIETIES.')
      END SELECT
      CLOSE(1)

      DEALLOCATE(CRIT3,CRIT2,CRIT2A,IYR,IVS,RM,S,IVN,VNAME,IMN,MNAME,
     1 AM,SD,YAV1,IOY1,IWY1,YASG,MISMATCH)

      RETURN
      END
C****************************************************************************
C
      SUBROUTINE UNIF3(TITLE,NM,IMN,MNAME,NV,VNAME,NY,IYR,NVS,IVS,RM,S,
     1    NOUT,RMV,IPRINT,CRIT3,CRIT2,CRIT2A,
     2    YAV1,YASG,YCRI3,YCRI2,YCRI2A,IOY1,IWY1,N1,ERRLEV,NPR,IVN)
C
C       FOR EACH OF NM CHARACTERS ,
C       APPLY OVER-YEARS UNIFORMITY CRITERION TO COMPARE THE BETWEEN-
C       PLANT STANDARD DEVIATIONS OF CANDIDATE VARIETIES AGAINST THOSE
C       OF ESTABLISHED REFERENCE VARIETIES ,
C       AND PRINT A SUMMARY .
C
C         AUTHORS - M.TALBOT, D.L.ROBINSON, S.E.LEONARD
C                     SCOTTISH AGRICULTURAL STATISTICS SERVICE
C                     UNIVERSITY OF EDINBURGH
C                      JANUARY 1989
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C
C       TITLE  - CHARACTER VARIABLE (A72) HOLDING TITLE TO BE PRINTED
C                     AS A HEADING TO OUTPUT
C          NM  - NO. OF MEASURES
C         IMN  - MEASURE CODE NUMBER
C       MNAME  - CHARACTER ARRAY (A8) HOLDING MEASURE NAME
C          NV  - NO. OF VARIETIES
C       VNAME  - CHARACTER ARRAY (A12) HOLDING NV VARIETY NAMES
C          NY  - NO. OF YEARS OR TRIALS
C         IYR  - INTEGER ARRAY HOLDING NY YEAR (OR TRIAL) IDENTIFIERS
C                TO BE PRINTED AS COLUMN HEADINGS
C         NVS  - NO. OF REFERENCE VARIETIES.
C                IF NVS=0 THEN ALL VARIETIES ARE ASSUMED TO BE
C                REFERENCE VARIETIES.
C                (IF NVS=0 THEN IT IS ASSUMED THAT ALL VARIETIES
C                    ARE STANDARD VARIETIES)
C         IVS  - INTEGER ARRAY HOLDING THE ORDER POSITION IN VNAME
C                OF THE REFERENCE VARIETIES. IF NVS=0 THEN THE
C                VALUES IN THIS ARRAY NEED NOT BE SET.
C          RM  - SINGLE-DIMENSION ARRAY HOLDING CHARACTER MEANS IN
C                     VARIETY ORDER WITHIN YEAR
C           S  - SINGLE-DIMENSION ARRAY HOLDING STANDARD DEVIATIONS IN
C                     VARIETY ORDER WITHIN YEAR
C        NOUT  - STREAM NUMBER TO WHICH OUTPUT TO BE PRINTED
C         RMV  - VALUE GIVEN TO MISSING REAL OBSERVATIONS IN RM AND S
C       IPRINT - PRINT OPTION
C                 0 - ONLY SUMMARY TABLE
C                 1 - ALL TABLES (PLOTS 3 PER PAGE)
C                 2 - ALL TABLES (PLOTS 1 PER PAGE)
C                -1 - ALL TABLES (NO PLOTS)
C       CRIT3  - PROBABILITY FOR REJECTION IN 3-YEAR TEST
C       CRIT2  - PROBABILITY FOR REJECTION IN 2 YEAR TEST
C       CRIT2A - PROBABILITY FOR ACCEPTANCE IN 2 YEAR TEST
C        YAV1  - OUTPUT ARRAY (NM X NV) HOLDING ADJUSTED MEAN LOG(STANDARD
C                  DEVIATION+1)
C        YASG  - OUTPUT GRAND MEAN ADJ. LOG(SD+1) FOR REFERENCE VARIETIES
C       YCRI3  - OUTPUT REJECT CRITICAL LEVEL FOR 3-YEAR CRITERION
C       YCRI2  - OUTPUT REJECT CRITICAL LEVEL FOR 2-YEAR CRITERION
C       YCRI2A - OUTPUT ACCEPT CRITICAL LEVEL FOR 2-YEAR CRITERION
C        IOY1  - OUTPUT ARRAY (NM X NV) INDICATING OVER-YEARS UNIFORMITY STATUS
C                 1 - UNIFORMITY ACCEPTABLE ON OVER-YEARS CRITERION
C                 2 - SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION, AFTER
C                     3 YEARS
C                 3 - SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION AFTER
C                     2 YEARS
C                 4 - AFTER 2 YEARS THE SD DOES NOT EXCEED THE 2 YEAR
C                      CRITERION BUT IS STILL TOO LARGE TO ACCEPT AND
C                       SHOULD BE TESTED FOR FURTHER YEAR
C        IWY1  - OUTPUT ARRAY (NM X NV) INDICATING WITHIN-YEAR
C                UNIFORMITY OF VARIETIES
C                 1   - UNIFORMITY ACCEPTABLE ON UPOV WITHIN-YEAR CRITERION
C                 N+1 - SD EXCEEDS 1.265 X MEAN OF REFERENCE VARIETIES
C                       IN EACH OF N YEARS
C        N1    - INPUT  NV*NY*NM
C        ERRLEV         ERROR REPORTING CODE
C        NPR    - CODE TO CONTROL OUTPUT WIDTH
C                 0 - FOR 80 COLUMN O/P
C                 1 - FOR 120 COLUMN O/P

C *** Refs to NY and NY-1 changed to 3 and 2 years respectively for Crits ***
C *** YCRI* and IOY1 parameter value meanings by IMN 25.3.96 ***
C *** N1 as argument added on 2/12/97 IMN ***
C *** ERRLEV and NPR added as arguments 5/12/97 IMN ***
C
C******************************************************************************
      REAL, DIMENSION(N1)::RM,S
      REAL, DIMENSION(NM)::YASG
      REAL, DIMENSION(:), ALLOCATABLE::RM1,S1,YAV
      INTEGER, DIMENSION(NV)::IVS,IVN
      INTEGER, DIMENSION(NM)::IMN
      INTEGER, DIMENSION(NY)::IYR
      INTEGER, DIMENSION(:), ALLOCATABLE::IOY,IWY,IWK
      CHARACTER(LEN=12), DIMENSION(NV)::VNAME
      CHARACTER(LEN=8), DIMENSION(NM)::MNAME
      REAL, DIMENSION(NM,NV)::YAV1
      INTEGER, DIMENSION(NM,NV)::IOY1,IWY1
      CHARACTER*72 TITLE
      CHARACTER*2 ICHAR(4),INUMB(6)
      CHARACTER*1 FF
      INTEGER ERRLEV,NPR,NCOL,NROW
      DATA ICHAR/' ','*','+',':'/,INUMB/' ','1','2','3','4','5'/
C
C    +++++ Check if data too large
C

C *** N2 and N3 introduced by IMN 25.3.96 ***

      N2=2
      N3=3
      NY1=NY-1
      N=NV*NY
      N1=N*NM
C
C     +++++ IF NVS IS SET TO ZERO THEN TAKE ALL VARIETIES AS STANDARDS
C
      IF(NVS.EQ.0) THEN
        DO 21 I=1,NV
          IVS(I)=I
21      CONTINUE
        NVS=NV
      ENDIF
C
C     +++++ DO FOR EACH MEASURE
C
      ALLOCATE(RM1(N),S1(N),YAV(NV),IOY(NV),IWY(NV))

      DO 90 I=1,NM
        DO 75 J=1,N
          K=(I-1)*N+J
          S1(J)=S(K)
          RM1(J)=RM(K)
75      CONTINUE
C
C     +++++ DO UNIFORMITY ANALYSIS
C
        MCODE=IMN(I)

        CALL UNIFA(TITLE,MNAME(I),NV,VNAME,NY,IYR,NVS,IVS,RM1,S1,NOUT,
     1           RMV,IPRINT,CRIT3,CRIT2,CRIT2A,YAV,YASG(I),
     2        YCRI3,YCRI2,YCRI2A,IOY,IWY,MCODE,N,ERRLEV,NPR,IVN)

C *** MCODE added to hold character no. for current measure IMN 03.02.97 ***

        DO 87 J=1,NV
          YAV1(I,J)=YAV(J)
          IOY1(I,J)=IOY(J)
          IWY1(I,J)=IWY(J)
87      CONTINUE
90    CONTINUE

C     +++++ Print for each variety with SD expressed as ratio of
C           reference varieties.
C
      ALLOCATE(IWK(NM))

      IF (IPRINT.GT.0) WRITE(NOUT,1013)
1013  FORMAT(1X)

C ***************************************************************************
C        FF CONTROLS PAGE THROWS
C        NCOL IS NUMBER OF COLUMNS IN WIDE TABLES
C        NROW IS NUMBER OF ROWS IN A TABLE
C ***************************************************************************

      IF (NPR.EQ.1) THEN
         FF='1'
         NCOL=17
         NROW=55
      ELSE
         FF=CHAR(12)
         NCOL=10
         NROW=55
      ENDIF

      NROW=NROW-14
C
C     ++++++ Print summary
C
      DO 88 KK=1,NM,NCOL
      DO 86 KL=1,NV,NROW
            KA=MIN((KK+NCOL-1),NM)
            KB=MIN((KL+NROW-1),NV)
            IF (NPR.EQ.0) THEN
                WRITE(NOUT,1010) FF,TITLE,(IMN(I),I=KK,KA)
            ELSE
                WRITE(NOUT,1018) FF,TITLE,(IMN(I),I=KK,KA)
            ENDIF
1010        FORMAT(A1/2X,A72,///,5X,
     1      '**** OVER-YEARS UNIFORMITY ANALYSIS ','SUMMARY ****'
     2      ,//,12X,'WITHIN-PLOT STANDARD DEVIATIONS AS % ',
     3      'MEAN OF REFERENCE VARIETY SDS',//,22X,'CHARACTER ',
     4      'NUMBER',//,17X,10I6)

1018        FORMAT(A1/2X,A72,///,5X,
     1      '**** OVER-YEARS UNIFORMITY ANALYSIS ','SUMMARY ****'
     2      ,//,12X,'WITHIN-PLOT STANDARD DEVIATIONS AS % ',
     3      'MEAN OF REFERENCE VARIETY SDS',//,22X,'CHARACTER ',
     4      'NUMBER',//,17X,17I6)

            DO 85 J=KL,KB
                  IF (J.EQ.1) WRITE(NOUT,2014)
2014              FORMAT(' REFERENCE',/)
                  IF (J.EQ.(NVS+1)) WRITE(NOUT,2015)
2015              FORMAT(/,' CANDIDATE',/)

                  IF (J.LE.NVS) THEN
                     NREF=0
                     DO 2020 KJ=1,NV
                             DO 2030 MJ=1,NVS
                                     IF (KJ.EQ.IVS(MJ)) THEN
C                                      * found a reference variety *
                                        NREF=NREF+1
                                        IF (NREF.EQ.J) THEN
                                           LL=KJ
                                           GOTO 111
                                        ENDIF
                                     ENDIF
2030                         CONTINUE
2020                 CONTINUE
                  ELSE
                     NCAN=0
                     DO 2040 KJ=1,NV
                             DO 2050 MJ=1,NVS
                                     IF (KJ.EQ.IVS(MJ)) THEN
C                                      * found a reference variety *
                                         GOTO 2040
                                     ENDIF
2050                         CONTINUE
                             NCAN=NCAN+1
                             IF (NCAN.EQ.(J-NVS)) THEN
                                LL=KJ
                                GOTO 111
                             ENDIF
2040                 CONTINUE
                  ENDIF

111           DO 82 I=KK,KA
                IWK(I)=-1
                IF((YASG(I).NE.0).AND.(YAV1(I,LL).NE.RMV).AND.
     1            (YAV1(I,LL).GT.0)) THEN
                  IWK(I)=(YAV1(I,LL)/YASG(I))*100+0.5
                ENDIF
82            CONTINUE
                IF (NPR.EQ.0) THEN
                   WRITE(NOUT,1011) IVN(LL),VNAME(LL),(IWK(I),
     +              ICHAR(IOY1(I,LL)), INUMB(IWY1(I,LL)),I=KK,KA)
                ELSE
                   WRITE(NOUT,1017) IVN(LL),VNAME(LL),(IWK(I),
     +              ICHAR(IOY1(I,LL)), INUMB(IWY1(I,LL)),I=KK,KA)
                ENDIF
1011          FORMAT(1X,I4,1x,A12,1X,10(I4,2A1))
1017          FORMAT(1X,I4,1x,A12,1X,17(I4,2A1))
85          CONTINUE

  86  CONTINUE
  88  CONTINUE

      IF ((KB-KL).LE.22) WRITE(NOUT,'(A1)') FF

      WRITE(NOUT,1014)
1014  FORMAT(//,5X,'CHARACTER KEY : ',/)
      WRITE(NOUT,1015) (IMN(I),MNAME(I),I=1,NM)
1015  FORMAT(15X,I5,2X,A8,I5,2X,A8)
      WRITE(NOUT,1012) N3,CRIT3,N2,CRIT2,N2,CRIT2A

C ** NY and NY-1 replaced by N3 and N2 respectively in above line IMN 25.3.96 **

1012  FORMAT(//,4X,'SYMBOLS :',//,5X,'* - SD EXCEEDS OVER-YEARS CRITERIO
     1N AFTER',I2,' YEARS WITH PROBABILITY ',F7.4,/,5X,'+ - SD EXCEEDS O
     2VER-YEARS CRITERION AFTER',I2,' YEARS WITH PROBABILITY ',F7.4,/,
     35X,': - SD NOT YET ACCEPTABLE AFTER', I2 ,' YEARS WITH PROBABILITY
     4 ',F7.4,/, 1X ,'1,2,3 - THE NUMBER OF OCCASIONS THE WITHIN-YEARS S
     5D EXCEEDS THE UPOV CRITERION',/)
      GOTO 999
999   DEALLOCATE(RM1,S1,YAV,IOY,IWY,IWK)
      RETURN
      END
C
C****************************************************************************
C
      SUBROUTINE UNIFA(TITLE,MNAME,NV,VNAME,NY,IYR,NVS,IVS,RM,S,
     1    NOUT,RMV,IPRINT,CRIT3,CRIT2,CRIT2A,
     2    YAV,YASG,YCRI3,YCRI2,YCRI2A,IOY,IWY,MCODE,N,ERRLEV,NPR,IVN)

C
C       FOR A SINGLE CHARACTER ,
C       APPLY OVER-YEARS UNIFORMITY CRITERION TO COMPARE THE BETWEEN-
C       PLANT STANDARD DEVIATIONS OF CANDIDATE VARIETIES AGAINST THOSE
C       OF ESTABLISHED REFERENCE VARIETIES .
C
C         AUTHORS - M.TALBOT, D.L.ROBINSON, S.E.LEONARD
C                      SASS, EDINBURGH, JANUARY 1989
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C
C       TITLE  - CHARACTER VARIABLE (A72) HOLDING TITLE TO BE PRINTED
C                     AS A HEADING TO OUTPUT
C       MNAME  - CHARACTER VARIABLE (A8) HOLDING MEASURE NAME
C          NV  - NO. OF VARIETIES
C       VNAME  - CHARACTER ARRAY (A12) HOLDING NV VARIETY NAMES
C          NY  - NO. OF YEARS OR TRIALS
C         IYR  - INTEGER ARRAY HOLDING NY YEAR (OR TRIAL) IDENTIFIERS
C                TO BE PRINTED AS COLUMN HEADINGS
C         NVS  - NO. OF REFERENCE VARIETIES
C         IVS  - INTEGER ARRAY HOLDING THE ORDER POSITION IN VNAME
C                     OF THE REFERENCE VARIETIES
C          RM  - SINGLE-DIMENSION ARRAY HOLDING CHARACTER MEANS IN
C                     VARIETY ORDER WITHIN YEAR
C           S  - SINGLE-DIMENSION ARRAY HOLDING STANDARD DEVIATIONS IN
C                     VARIETY ORDER WITHIN YEAR
C        NOUT  - STREAM NUMBER TO WHICH OUTPUT TO BE PRINTED
C         RMV  - VALUE GIVEN TO MISSING REAL OBSERVATIONS IN RM AND S
C      IPRINT  - PRINT OPTION
C                  0 - ONLY SUMMARY TABLE
C                  1 - ALL TABLES (PLOTS 3 PER PAGE)
C                  2 - ALL TABLES (PLOTS 1 PER PAGE)
C                 -1 - ALL TABLES - NO PLOTS
C       CRIT3  - PROBABILITY FOR REJECTION IN 3-YEAR TEST
C       CRIT2  - PROBABILITY FOR REJECTION IN 2 YEAR TEST
C       CRIT2A - PROBABILITY FOR ACCEPTANCE IN 2 YEAR TEST
C         YAV  - OUTPUT ARRAY HOLDING ADJUSTED MEAN LOG(STANDARD DEVIATION+1)
C                FOR THE NV VARIETIES
C        YASG  - OUTPUT GRAND MEAN ADJ. LOG(SD+1) FOR REFERENCE VARIETIES
C       YCRI3  - OUTPUT REJECT CRITICAL LEVEL FOR 3-YEAR CRITERION
C       YCRI2  - OUTPUT REJECT CRITICAL LEVEL FOR 2-YEAR CRITERION
C       YCRI2A - OUTPUT ACCEPT CRITICAL LEVEL FOR 2-YEAR CRITERION
C         IOY  - OUTPUT ARRAY INDICATING OVER-YEARS UNIFORMITY STATUS OF NV
C                VARIETIES
C                 1 - UNIFORMITY ACCEPTABLE ON OVER-YEARS CRITERION
C                 2 - SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION, AFTER
C                     3 YEARS
C                 3 - SD EXCEEDS OVER-YEARS UNIFORMITY CRITERION AFTER
C                     2 YEARS
C                 4 - AFTER 2 YEARS THE SD DOES NOT EXCEED THE 2
C                      CRITERION BUT IS STILL TOO LARGE TO ACCEPT AND
C                       SHOULD BE TESTED FOR FURTHER YEAR
C         IWY  - OUTPUT ARRAY INDICATING WITHIN-YEAR UNIFORMITY STATUS OF
C                VARIETIES
C                 1   - UNIFORMITY ACCEPTABLE ON UPOV WITHIN-YEAR CRITERION
C                 N+1 - SD EXCEEDS 1.265 X MEAN OF REFERENCE VARIETIES IN EACH
C                       OF N YEARS
C         N    - INPUT  NV*NY
C         ERRLEV - ERROR REPORTING CODE
C        NPR    - CODE TO CONTROL OUTPUT WIDTH
C                 0 - FOR 80 COLUMN O/P
C                 1 - FOR 120 COLUMN O/P

C *** MCODE added to hold character no. for current measure  IMN 03.02.97 ***

C *** Refs to NY and NY-1 changed to 3 and 2 years respectively for Crits ***
C *** YCRI* and IOY1 parameter value meanings by IMN 25.3.96 ***
C *** N added on 2/12/97 by IMN ***
C *** ERRLEV added as an argument 5/12/97 IMN ***C
C
C
C     Adding Bit to program to ensure EXP(<-80) is set to 0
C     Required for Windows 3.1 running the Win32s. John Ward 26/5/98
C
C******************************************************************************
      REAL, DIMENSION(N)::RM,S
      REAL, DIMENSION(NV)::YAV
      CHARACTER(LEN=12), DIMENSION(NV)::VNAME
      INTEGER, DIMENSION(NY)::IYR
      INTEGER, DIMENSION(NV)::IVS,IVN
      INTEGER, DIMENSION(NV)::IOY,IWY
      INTEGER, DIMENSION(:), ALLOCATABLE::OLDORD,NYV,NYY,IX,ITEMP,IP
      REAL, DIMENSION(:), ALLOCATABLE::X,Y,FIT,YA,YSM,XSM,XS1,YS1,GWK,
     1 YAS,YASM,YV,STD,XV,YY,XY,TEMP,GX,GY
      REAL AMS(4)
C     INSERTED BY JOHN WARD 26/5/98
      REAL Bit
      INTEGER IDF(4),ERRLEV,NPR

      INTEGER LPCNT,ISPLIT
      LOGICAL SPLIT
      CHARACTER*2 CROSS(6)
      CHARACTER*72 TITLE
      CHARACTER*1 FF
      CHARACTER(LEN=8) MNAME
      DATA CROSS/' ','*','X','+','#',':'/
C
C     ++++ Set program constants
C
C        MAXITR = max. no. of iterations in FCPD1
C        IMV    = value given to missing integer observations
C        IQUANT = quantile
C        MASPAN = no. of observations used to calculate MA
C        MINSP  = no. of observations used at ends
C        FF - causes page throw

      MAXITR=10
      IMV=-1
      IQUANT=50
      MASPAN=9
      MINSP=3
      IF (NPR.EQ.1) THEN
         FF='1'
      ELSE
         FF=CHAR(12)
      ENDIF

C *** N2 and N3 introduced by IMN 25.3.96 ***

      N2=2
      N3=3
C
C    +++++ Check if data too large
C
      N=NV*NY
      N1=N
C
C     +++++ Array IVS holds position of reference varieties in std list.
C           Extend so that also contains positions of candidates.
C

      ALLOCATE(OLDORD(NV))

      DO 2 J=1,NV
        OLDORD(J)=0
2     CONTINUE
      DO 4 J=1,NVS
        OLDORD( IVS(J) ) = J
4     CONTINUE
      I=0
      DO 6 J=1,NV
        IF( OLDORD(J) .NE. 0 ) GO TO 6
        I=I+1
        IVS(NVS+I)=J
6     CONTINUE
      NCAND=NV-NVS
      IF(I.NE.NCAND) GO TO 997
C
C    +++++   Set up work arrays
C
              ALLOCATE(X(N),Y(N))

        DO 20 J=1,N
          X(J)=RM(J)
          Y(J)=S(J)
20      CONTINUE
C
C      +++++ Convert sd to log(sd+1)
C
      DO 30 J=1,N
        IF(Y(J).GT.RMV) Y(J)=LOG(Y(J)+1)
30    CONTINUE
      KNT1=0
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     +++++ CALCULATE MOVING AVERAGE FIT FOR EACH YEAR AND REMOVE
C     +++++ MOVING AVERAGE RELATIONSHIP FROM ACTUAL SDS .
C
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      NYTREB=NY*3
      NVDOUB=NV*2
      ALLOCATE(FIT(N),YA(N),IP(N),YSM(NY),XSM(NY),XS1(NV),YS1(NV),
     1 GWK(NVDOUB),YAS(N),NYV(NV),YASM(NY),NYY(NY),TEMP(NYTREB),
     2 ITEMP(NYTREB),GX(NV),GY(NVDOUB),STD(NY))

      DO 170 J=1,NY
        STD(J)=0.0
        K=0
        K1=0
        IBY=(J-1)*NV
C
C       +++++ Initialise arrays.
C
        DO 40 I=1,NV
          FIT(IBY+I)=RMV
          YA(IBY+I)=RMV
          IP(IBY+I)=0
40      CONTINUE
        YSM(J)=0.0
        XSM(J)=0.0
C
C   +++++  Extract data for reference varieties, sort and calc means
C
        DO 50 I=1,NVS
          IPT=IBY+IVS(I)
          IF( X(IPT).GT.RMV) THEN
               K1=K1+1
               XSM(J)=XSM(J)+X(IPT)
          ELSE
               YA(IPT)=RMV
               GO TO 50
          ENDIF
          IF( Y(IPT).LE.RMV ) GO TO 50
          K=K+1
          OLDORD(K)=IPT
          XS1(K)=X(IPT)
          YSM(J)=YSM(J)+Y(IPT)
50      CONTINUE
        IF(K1.GT.0) THEN
           XSM(J)=XSM(J)/K1
        ELSE
           XSM(J)=RMV
           GO TO 52
        ENDIF
        IF(K.GT.0) THEN
           YSM(J)=YSM(J)/K
        ELSE
           YSM(J)=RMV
           GO TO 52
        ENDIF
        IF(K.GE.MASPAN) GO TO 55
C
C     +++++ INSUFFICIENT REFERENCE VARIETIES , SO UNADJUSTED
C     +++++ DATA WILL BE USED FOR THIS YEAR .
C
52        DO 53 I=1,NV
            YA(IBY+I)=Y(IBY+I)
53        CONTINUE
          GOTO 155
C
C         +++++ ORDER ON CHARACTER MEANS
C
55        CALL SORTR(K,XS1,OLDORD)
C
C         +++++ Extract Y data to same order
C
          DO 60 I=1,K
            YS1(I) = Y( OLDORD(I) )
60        CONTINUE
C
C         +++++ Calculate MA relationship for reference varieties
C
          CALL MAFIT(K,XS1,YS1,GWK,MASPAN,MINSP)
C
C         +++++ Transfer fitted values to array FIT, adjusted to YA
C
          DO 70 I=1,K
            FIT( OLDORD(I) ) = GWK(I)
            YA( OLDORD(I) ) =  Y( OLDORD(I) ) - GWK(I) + YSM(J)
70        CONTINUE
C
C         +++++ Calculate fitted values for candidates as linear
C               interpolation of nearest two reference varieties
C
          DO 100 ICAND=1,NCAND
            IPT=IBY + IVS(NVS+ICAND)
            XC=X(IPT)
            YC=Y(IPT)
            IF( YC.LE.RMV ) GO TO 100
            IF( XC.LE.RMV ) GO TO 100
            DO 75 I=1,K
              IF( XC .LT. XS1(I) ) GO TO 80
75          CONTINUE
            F=GWK(K)
            GO TO 90
80          IF( I.LE.1 ) THEN
                F=GWK(I)
            ELSE
                D1=XS1(I)-XC
                D2=XC-XS1(I-1)
                IF( D1.EQ.0 ) THEN
                      F=GWK(I)
                      GO TO 90
                ELSE
                      F=(D2*GWK(I)+D1*GWK(I-1))/(D1+D2)
                ENDIF
            ENDIF
90          FIT(IPT)=F
            YA(IPT)=Y(IPT)-F+YSM(J)
100         CONTINUE
C
C     +++++ TRANSFER ADJUSTED SDS FOR REFERENCE VARIETIES INTO V X Y TABLE
C
155     DO 160 K=1,NVS
          M=(J-1)*NV+IVS(K)
          YAS(KNT1+K)=YA(M)
160     CONTINUE

          NCOUNT=0
          DO 99 IPOSIT=1,NVS
                IPT=IBY+IVS(IPOSIT)
                YC=Y(IPT)
                IF (YC.GT.RMV) THEN
                   IF (YC.LT.-80.) THEN
                      bit=0.
                   ELSE
                      bit=EXP(YC)
                   ENDIF
                   STD(J)=STD(J)+bit-1
                   NCOUNT=NCOUNT+1
            ENDIF
99        CONTINUE
            IF (NCOUNT.GT.0) STD(J)=STD(J)/NCOUNT*1.265
      KNT1=KNT1+NVS
170   CONTINUE
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     +++++ CALCULATE CRITICAL VALUES FROM ADJUSTED SDS
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     +++++ Do anova for ajusted log sds of reference varieties
C
      ALLOCATE(YV(NV),XV(NV),YY(NY),XY(NY),IX(NV))

      N1=NVS*NY
      CALL FCPD1(YAS,N1,NVS,NY,0.0,RMV,MAXITR,IRETC,YAV,NYV,YASM,
     1            NYY,YASG,AMS,IDF)


C ***IMN INSERTED 4/5/98 SO THAT ADJUSTED OVERALL MEAN COMPUTED ***

      NCNT=0
      YASG=0.0
      DO 888 III=1,NVS
             IF (YAV(III).GT.RMV) THEN
                NCNT=NCNT+1
                YASG=YASG+YAV(III)
             ENDIF
888   CONTINUE
      YASG=YASG/NCNT
C
C     +++++ Pool variety and variety x year mean squares
C
      IDF2=IDF(2)+IDF(3)
      S2=0
      IF((IDF2.GT.0).AND.(AMS(3).GT.0)) S2=(AMS(2)*IDF(2)+AMS(3)*IDF(3))
     1                                         /IDF2
C
C     +++++ Calculate critical t value
C
      CALL TABT(CRIT3*2.0,T3,IDF2,0.001)
      CALL TABT(CRIT2*2.0,T2,IDF2,0.001)
      CALL TABT(CRIT2A*2.0,T2A,IDF2,0.001)

C
C     +++++ Estimate median
C
      DO 200 K=1,NVS
        YV(K)=YAV(K)
200   CONTINUE
      CALL SORT(YV,NVS)
      MY1=INT((IQUANT/100.)*NVS+0.99)
      MY2=INT((IQUANT/100.)*NVS+1.00)
      QUANT=0.5*(YV(MY1)+YV(MY2))
C
C     +++++ Calculate over-years uniformity criterion
C

C *** Refs to NY and NY-1 changed to N3 and N2 by IMN on 25.3.96 ***
C *** Critical values calculated with sed using no. of ref varieties from
C *** as many years as possible rather than N2*NVS or N3*NVS

      CDIF3=T3*SQRT(S2*(1.0/N3+1.0/(IDF(4)+1)))
      CDIF2=T2*SQRT(S2*(1.0/N2+1.0/(IDF(4)+1)))
      CDIF2A=T2A*SQRT(S2*(1.0/N2+1.0/(IDF(4)+1)))

      YCRI3=YASG+CDIF3
      YCRI2=YASG+CDIF2
      YCRI2A=YASG+CDIF2A
C
C      +++++ Old section for deriving UPOV within-year uniformity criterion
C      +++++ based on adjusted sd was removed by IMN on 29/9/98
C      DO 220 J=1,NY
C        STD(J)=0
C        KNT=0
C        DO 210 K=1,NVS
C          L=(J-1)*NVS+K
C          IF(YAS(L).GT.RMV) THEN
C     INSERTING THE NEXT 6 LINES JOHN WARD 26/5/98
C            IF (YAS(L).LT.-80.) THEN
C              Bit=0.
C            ELSE
C              Bit=EXP(YAS(L))
C            ENDIF
C            STD(J)=STD(J)+Bit-1.
CC           STD(J)=STD(J)+EXP(YAS(L))-1.
C            KNT=KNT+1
C          ENDIF
C210     CONTINUE
C        IF(KNT.GT.0) STD(J)=STD(J)*1.265/KNT
C220   CONTINUE
C
C     +++++ Remove year differences from adjusted LOG SDs
C
      DO 230 J=1,NY
      DO 231 K=1,NV
        L=(J-1)*NV+K
        IF(YA(L).GT.RMV) YA(L)=YA(L)-YASM(J)
231   CONTINUE
230   CONTINUE
C
C     +++++ Calculate means over years for all varieties
C
      CALL MEAN1(Y,N,NV,NY,RMV,YV,NYV,YY,NYY,GMN,KNT)
      CALL MEAN1(YA,N,NV,NY,RMV,YAV,NYV,YY,NYY,GMN,KNT)
      CALL MEAN1(X,N,NV,NY,RMV,XV,NYV,XY,NYY,GMN,KNT)
C
C     +++++ Rank varieties in ascending order of character mean
C
      CALL RANK(XV,NV,1,RMV,IMV,IX)
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     +++++ PRINT UNIFORMITY RESULTS FOR SINGLE CHARACTER
C
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     +++++ Print heading to table
C
      ISPLIT=0
      IF ((NY.GE.4).AND.(NPR.EQ.0)) ISPLIT=1
      IF ((NY.GE.4).AND.(NY.LE.5).AND.(NPR.EQ.1)) ISPLIT=2
      IF ((NY.GE.6).AND.(NPR.EQ.1)) ISPLIT=3
      IF ((ISPLIT.EQ.1).OR.(ISPLIT.EQ.3)) THEN
         SPLIT=.TRUE.
      ELSE
         SPLIT=.FALSE.
      ENDIF

      IF (IPRINT .NE. 0) THEN
      WRITE(NOUT,1015)FF,TITLE,MCODE,MNAME

C *** Change to print out character number and also full title by IMN 3.3.97 ***

1015  FORMAT(A1/2X,A72,/,34X,I4,' - ',A8)
      WRITE(NOUT,1010)
1010  FORMAT(//,5X, '**** UNIFORMITY ANALYSIS OF BETWEEN-PLANT STANDARD
     1DEVIATIONS (SD) ****')
      IF(NY.EQ.2) THEN
        WRITE(NOUT,1007)
1007    FORMAT(/ ,24X,'OVER-YEARS',12X,'INDIVIDUAL YEARS',/,18X,
     1  '--------------------- --------------------------',
     3  /,'  AFP VARIETY       CHAR.  ADJ.  UNADJ  CHARACTER MEAN   ',
     4  'LOG(SD+1) ')
      ELSE IF(NY.EQ.3)THEN
        WRITE(NOUT,1008)
1008    FORMAT(/  ,24X,'OVER-YEARS',17X,'INDIVIDUAL YEARS',/,18X,
     1  '--------------------   -----------------------------------',
     2  /,
     3  '  AFP VARIETY       CHAR.  ADJ.  UNADJ   - CHARACTER MEAN - ',
     4  ' - LOG (SD+1) -')
      ELSE IF(ISPLIT.EQ.2) THEN
        IF (NY.EQ.4) WRITE(NOUT,1009)
1009    FORMAT(/  ,24X,'OVER-YEARS',22X,'INDIVIDUAL YEARS',/,17X,
     1  '---------------------   -----------------------------------',
     2  '------------',/,
     3  '  AFP VARIETY       CHAR.  ADJ.  UNADJ   ',
     4  '---  CHARACTER MEAN ---','    ---- LOG(SD+1) ----')
        IF (NY.EQ.5) WRITE(NOUT,3010)
3010    FORMAT(/  ,24X,'OVER-YEARS',32X,'INDIVIDUAL YEARS',/,18X,
     1  '---------------------  -----------------------------------',
     2  '------------------------',/,
     3  '  AFP VARIETY       CHAR.  ADJ.  UNADJ  ',
     4  ' ------  CHARACTER MEAN ------',
     5  '  --------- LOG(SD+1) --------')
      ELSE IF ((ISPLIT.EQ.1).OR.(ISPLIT.EQ.3)) THEN
        WRITE(NOUT,2000)
2000    FORMAT(/,19X,'OVER-YEARS',/19X,'----------',/,
     1  '  AFP VARIETY       CHAR    ADJ    UNADJ')
      ENDIF

      IF ((ISPLIT.EQ.0).OR.(ISPLIT.EQ.2)) THEN
         WRITE(NOUT,1011)(IYR(J),J=1,NY),(IYR(J),J=1,NY)
1011     FORMAT(20X,'MEAN  LOG SD LOG SD ',12I6,/,40X,12I6)
      ELSE
        WRITE(NOUT,2001)
2001    FORMAT(20X,'MEAN  LOG SD  LOG SD')
      ENDIF
      WRITE(NOUT,*)
      ENDIF
C
C   +++++ Print reference varieties in order of character mean
C
      LPCNT=0
289   CONTINUE
      IF (IPRINT.NE.0) THEN
      IF ((ISPLIT.EQ.1).OR.(ISPLIT.EQ.3)) LPCNT=LPCNT+1
      IF (LPCNT.EQ.2) THEN
         WRITE(NOUT,1015) FF,TITLE,MCODE,MNAME
         WRITE(NOUT,1010)
         WRITE(NOUT,2004)
2004     FORMAT(19X,'INDIVIDUAL YEARS - CHARACTER MEANS')
         IF (ISPLIT.EQ.1) WRITE(NOUT,2006) (IYR(J),J=1,NY)
2006     FORMAT(16X,8I7,/,16X,8I7)
2007     FORMAT(16X,14I7,/,16X,14I7)
         IF (ISPLIT.EQ.3) WRITE(NOUT,2007) (IYR(J),J=1,NY)
      ELSE IF (LPCNT.EQ.3) THEN
         WRITE(NOUT,1015) FF,TITLE,MCODE,MNAME
         WRITE(NOUT,1010)
         WRITE(NOUT,2005)
2005     FORMAT(19X,'INDIVIDUAL YEARS - LOG(SD+1)')
         IF (ISPLIT.EQ.1) WRITE(NOUT,2006) (IYR(J),J=1,NY)
         IF (ISPLIT.EQ.3) WRITE(NOUT,2007) (IYR(J),J=1,NY)
      ENDIF

      ENDIF

      IF((NV.GT.NVS).AND.(IPRINT.NE.0)) THEN
        WRITE(NOUT,1014)
1014    FORMAT(' REFERENCE',/)
      ENDIF

      DO 290 J=1,NV
        IOY(J)=1
        IWY(J)=1
290   CONTINUE
      DO 320 J=1,NV
             K=J
             GOTO 305
305     DO 310 L=1,NVS
          IF(K.EQ.IVS(L)) THEN
            DO 306 JJ=1,NY
              M=(JJ-1)*NV+K
              TEMP(JJ)=X(M)
              ITEMP(JJ)=1
306         CONTINUE
            DO 307 JJ=1,NY
              JJ1=JJ+NY
              M=(JJ-1)*NV+K
              TEMP(JJ1)=Y(M)
              ITEMP(JJ1)=1
C       INSERTING THE NEXT 6 LINES JOHN WARD 26/5/98
              IF (Y(M).LT.-80.) THEN
                Bit=0.
              ELSE
                Bit=EXP(Y(M))
              ENDIF
              IF((Y(M).GT.RMV).AND.((Bit-1.0).GT.STD(JJ))) THEN
C             IF((Y(M).GT.RMV).AND.((EXP(Y(M))-1.0).GT.STD(JJ))) THEN
                ITEMP(JJ1)=3
                IWY(K)=IWY(K)+1
              ENDIF
307         CONTINUE
            DO 308 JJ=1,NY
              JJ1=JJ+2*NY
              M=(JJ-1)*NV+K
              TEMP(JJ1)=YA(M)
              ITEMP(JJ1)=1
              IF(IP(M).EQ.1) ITEMP(JJ1)=5
308         CONTINUE
            M1=1

C *** Repaced "NYV(K).EQ.NY" by ".GE.N3" IMN 25.03.96 ***

            IF((NYV(K).GE.N3).AND.(YAV(K).GT.YCRI3)) THEN
              M1=2
              IOY(K)=2
            ENDIF

C *** Repaced "NYV(K).EQ.(NY-1)" by ".EQ.N2" IMN 25.03.96 ***

            IF((NYV(K).EQ.N2).AND.(YAV(K).GT.YCRI2)) THEN
              M1=4
              IOY(K)=3
            ENDIF

C *** Repaced "NYV(K).EQ.(NY-1)" by ".EQ.N2" IMN 25.03.96 ***

            IF((NYV(K).EQ.N2).AND.(YAV(K).LE.YCRI2).AND.
     1           (YAV(K).GT.YCRI2A)) THEN
              M1=6
              IOY(K)=4
            ENDIF
            JJ1=2*NY
            IF ((IPRINT .NE. 0).AND.(.NOT.SPLIT)) THEN
            WRITE(NOUT,1012) IVN(K),VNAME(K),XV(K),YAV(K),CROSS(M1),
     1      YV(K),(TEMP(M),CROSS(ITEMP(M)),M=1,JJ1)
1012        FORMAT(1X,I4,1x,A12,F6.1,F7.3,A1,F7.3,2X,12(F5.1,A1),/,36X,
     1               12(F5.1,A1))
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.1)) THEN
              WRITE(NOUT,2002) IVN(K),VNAME(K),XV(K),YAV(K),CROSS(M1),
     1         YV(K)
2002          FORMAT(1X,I4,1x,A12,F7.2,F7.3,A2,F6.3)
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.2)) THEN
              IF (ISPLIT.EQ.1) WRITE(NOUT,2008) IVN(K),VNAME(K),
     1           (TEMP(M),CROSS(ITEMP(M)),M=1,NY)
2008             FORMAT(1X,I4,1x,A12,8(F6.2,A1),/,13X,8(F6.2,A1))
              IF (ISPLIT.EQ.3) WRITE(NOUT,2009) IVN(K),VNAME(K),
     1           (TEMP(M),CROSS(ITEMP(M)),M=1,NY)
2009             FORMAT(1X,I4,1x,A12,14(F6.2,A1),/,13X,14(F6.2,A1))
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.3)) THEN
              IF (ISPLIT.EQ.1) WRITE(NOUT,2008) IVN(K),VNAME(K),
     1           (TEMP(M),CROSS(ITEMP(M)),M=(NY+1),JJ1)
              IF (ISPLIT.EQ.3) WRITE(NOUT,2009) IVN(K),VNAME(K),
     1           (TEMP(M),CROSS(ITEMP(M)),M=(NY+1),JJ1)
            ENDIF
            GOTO 320
          ENDIF
310     CONTINUE
320   CONTINUE
      IF (IPRINT.NE.0) WRITE(NOUT,*)
      IF (NV-NVS.GT.0) THEN
C
C    +++++ Print candidate varieties
C
        IF (IPRINT.NE.0) WRITE(NOUT,1013)
1013    FORMAT(' CANDIDATE',/)
        DO 340 J=1,NV
          DO 330 L=1,NVS
            IF(J.EQ.IVS(L)) GOTO 340
330       CONTINUE
           IOY(J)=1
           IWY(J)=1
          DO 336 JJ=1,NY
            M=(JJ-1)*NV+J
            TEMP(JJ)=X(M)
            ITEMP(JJ)=1
336       CONTINUE
          DO 337 JJ=1,NY
            JJ1=JJ+NY
            M=(JJ-1)*NV+J
            TEMP(JJ1)=Y(M)
            ITEMP(JJ1)=1
C       INSERTING THE NEXT 6 LINES JOHN WARD 26/5/98
            IF (Y(M).LT.-80.) THEN
              Bit=0.
            ELSE
              Bit=EXP(Y(M))
            ENDIF
            IF((Y(M).GT.RMV).AND.((Bit-1.0).GT.STD(JJ))) THEN
C           IF((Y(M).GT.RMV).AND.((EXP(Y(M))-1.).GT.STD(JJ))) THEN
              ITEMP(JJ1)=3
              IWY(J)=IWY(J)+1
            ENDIF
337       CONTINUE
          DO 338 JJ=1,NY
            JJ1=JJ+2*NY
            M=(JJ-1)*NV+J
            TEMP(JJ1)=YA(M)
            ITEMP(JJ1)=1
            IF(IP(M).EQ.1) ITEMP(JJ1)=5
338       CONTINUE
          M1=1
          IOY(J)=1

C *** Repaced "NYV(J).EQ.NY" by ".GE.N3" IMN 25.03.96 ***

          IF((NYV(J).GE.N3).AND.(YAV(J).GT.YCRI3)) THEN
            M1=2
            IOY(J)=2
          ENDIF


C *** Repaced "NYV(J).EQ.(NY-1)" by ".EQ.N2" IMN 25.03.96 ***

          IF((NYV(J).EQ.N2).AND.(YAV(J).GT.YCRI2)) THEN
            M1=4
            IOY(J)=3
          ENDIF

C *** Repaced "NYV(J).EQ.(NY-1)" by ".EQ.N2" IMN 25.03.96 ***

          IF((NYV(J).EQ.N2).AND.(YAV(J).LE.YCRI2).AND.
     1           (YAV(J).GT.YCRI2A)) THEN
            M1=6
            IOY(J)=4
          ENDIF
          JJ1=2*NY
          IF ((IPRINT .NE. 0).AND.(.NOT.SPLIT)) THEN
          WRITE(NOUT,1012) IVN(J),VNAME(J),XV(J),YAV(J),CROSS(M1),
     1           YV(J),(TEMP(M),CROSS(ITEMP(M)),M=1,JJ1)
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.1)) THEN
              WRITE(NOUT,2002) IVN(J),VNAME(J),XV(J),YAV(J),CROSS(M1),
     1         YV(J)
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.2)) THEN
              IF (ISPLIT.EQ.1) WRITE(NOUT,2008) IVN(J),VNAME(J),
     1           (TEMP(M),CROSS(ITEMP(M)),M=1,NY)
              IF (ISPLIT.EQ.3) WRITE(NOUT,2009) IVN(J),VNAME(J),
     1           (TEMP(M),CROSS(ITEMP(M)),M=1,NY)
            ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.3)) THEN
              IF (ISPLIT.EQ.1) WRITE(NOUT,2008) IVN(J),VNAME(J),
     1           (TEMP(M),CROSS(ITEMP(M)),M=(NY+1),JJ1)
              IF (ISPLIT.EQ.3) WRITE(NOUT,2009) IVN(J),VNAME(J),
     1           (TEMP(M),CROSS(ITEMP(M)),M=(NY+1),JJ1)
          ENDIF
340     CONTINUE
      ENDIF

C
C    +++++ Print means and critical values
C

      NY1=NY-1
      XSMG=0
      K1=0
      DO 350 J=1,NY
        IF(XSM(J).GT.RMV) THEN
          K1=K1+1
          XSMG=XSMG+XSM(J)
        ENDIF
        YASM(J)=YASG
  350 CONTINUE
        IF(K1.GT.0) XSMG=XSMG/K1

      IF ((IPRINT .NE. 0).AND.(.NOT.SPLIT)) THEN

      WRITE(NOUT,1021)XSMG,YASG,(XSM(J),J=1,NY),(YSM(J),J=1,NY)
1021  FORMAT(/,' MEAN OF',/,' REFERENCE        ',F6.1,F7.3,9X,12F6.1,
     1                 /,39X,12F6.1)
      ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.1)) THEN
        WRITE(NOUT,2003) XSMG,YASG
2003    FORMAT(/,' MEAN OF',/,' REFERENCE        ',F7.2,F7.3)
      ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.2)) THEN
        IF (ISPLIT.EQ.1) WRITE(NOUT,2010) (XSM(J),J=1,NY)
        IF (ISPLIT.EQ.3) WRITE(NOUT,2011) (XSM(J),J=1,NY)
2010    FORMAT(/,' MEAN OF',/,' REFERENCE  ',5X,8(F7.2),/,13X,8(F7.2))
2011    FORMAT(/,' MEAN OF',/,' REFERENCE  ',5X,14(F7.2),/,13X,14(F7.2))
      ELSE IF ((IPRINT.NE.0).AND.(LPCNT.EQ.3)) THEN
        IF (ISPLIT.EQ.1) WRITE(NOUT,2010) (YSM(J),J=1,NY)
        IF (ISPLIT.EQ.3) WRITE(NOUT,2011) (YSM(J),J=1,NY)
      ENDIF
C
C *** Replaced NY and NY-1 by N3 and N2 respectively in WRITE 1023 by IMN ***
C *** 25.3.96 ***

      IF ((IPRINT.NE.0).AND.((LPCNT.EQ.0).OR.(LPCNT.EQ.3))) THEN
      WRITE(NOUT,1023)N3,YCRI3,CRIT3,N2,YCRI2,CRIT2,N2,YCRI2A,CRIT2A
1023  FORMAT(/,' UNIFORMITY CRITERION',/,27X,'PROB. LEVEL',//,
     1I3,'-YEAR REJECTION ',F6.3,F10.3,//,
     2I3,'-YEAR REJECTION ',F6.3,F10.3,//,
     3I3,'-YEAR ACCEPTANCE ',F5.3,F10.3)
      R=0
      IF(AMS(3).GT.0) R=AMS(2)/AMS(3)
      WRITE(NOUT,1030)IDF(1),AMS(1),IDF(2),AMS(2),R,IDF(3),AMS(3),
     1IDF(4),AMS(4)
1030  FORMAT(//,5X,'**** ANALYSIS OF VARIANCE OF ADJUSTED LOG(SD+1) ****
     1',//,12X,'  DF     MS     F RATIO',//,'  YEARS     ',I4,F11.5,/,
     2'  VARIETIES ',I4,F11.5,F5.1,/,'  RESIDUAL  ',I4,F11.5,//,
     3'  TOTAL     ',I4,F11.5)
C
C *** Replaced NY and NY-1 by N3 and N2 respectively in WRITE 1032 by IMN ***
C *** 25.3.96 ***
C
      WRITE(NOUT,1032) N3,N2,N2
1032  FORMAT(//,5X,'SYMBOLS',//,9X,'* - SD EXCEEDS OVER-YEARS UNIFORMITY
     1 CRITERION AFTER',I2,' YEARS.',/,9X,'+ - SD EXCEEDS OVER-YEARS UNI
     2FORMITY CRITERION AFTER',I2,' YEARS.',/, 9X ,': - SD NOT YET ACCEP
     3TABLE ON OVER-YEARS CRITERION AFTER',I2,' YEARS.',/,8X,
     4' X - SD EXCEEDS 1.265 TIMES MEAN OF REFERENCE VARIETIES',/,8X,
     5' # - WARNING - SD, CHARACTER MEAN, OR ADJUSTED SD IS UNUSUAL .')

      ENDIF

      IF ((LPCNT.NE.0).AND.(LPCNT.NE.3)) GOTO 289

      IF(IPRINT.GT.0)THEN
C
C   +++++ Plot LOG SD against mean for each year
C
          IUX=-999
          IUY=-999
          ISX=999
          ISY=999
C
C     +++++ Find min and max for plots
C
          DO 410 K=1,N
            IF((Y(K).LE.RMV).OR.(X(K).LE.RMV)) GOTO 410
            IF(ISX.GT.INT(X(K)))ISX=INT(X(K))
            IF(IUX.LT.INT(X(K)))IUX=INT(X(K))
            IF(ISY.GT.INT(FIT(K)))ISY=INT(FIT(K))
            IF(ISY.GT.INT(Y(K)))ISY=INT(Y(K))
            IF(IUY.LT.INT(Y(K)))IUY=INT(Y(K))
            IF(IUY.LT.INT(FIT(K)))IUY=INT(FIT(K))
410       CONTINUE
          UX=FLOAT(IUX)+1
          UY=FLOAT(IUY)+1
          SX=FLOAT(ISX)
          SY=FLOAT(ISY)
          IY1=1
          IX1=1
          IWD=35
          IDP=15
          IF(IPRINT.EQ.2) THEN
            IWD=55
            IDP=30
          ENDIF
C
C     +++++ Print scatter plot for each year
C
          DO 440 J=1,NY
            IF((IPRINT.EQ.2).OR.(MOD(J,3).EQ.1)) THEN
              WRITE(NOUT,1015)FF,TITLE,MCODE,MNAME
              WRITE(NOUT,1040)
1040          FORMAT(/,5X,'**** PLOT OF LOG (SD+1) AND CHARACTER MEAN',
     1               /)
            ENDIF
            DO 420 K=1,NV
              M=(J-1)*NV+K
              GX(K)=X(M)
              GY(K)=Y(M)
              GY(K+NV)=FIT(M)
420         CONTINUE
            WRITE(NOUT,1016)IYR(J)
1016        FORMAT(20X,'YEAR ',I4)
            WRITE(NOUT,1017)
1017        FORMAT(1X,'LOG (SD+1)')
            CALL SCAT(GX,GY,GWK,NV,NV*2,2,IWD,IDP,10,10,1.0,RMV,
     1                   NOUT,IX1,IY1,SX,UX,SY,UY)
            WRITE(NOUT,*)
440       CONTINUE
          WRITE(NOUT,1018)MNAME
1018      FORMAT(10X,'MEAN ',A8)
        ENDIF
      DEALLOCATE(X,Y,FIT,YA,IP,YSM,XSM,XS1,YS1,GWK,YAS,NYV,
     1 YASM,NYY,TEMP,ITEMP,GX,GY,YV,STD,XV,YY,XY,IX)
      GOTO 999
997   WRITE(NOUT,1004)
1004  FORMAT(1X,'PLEASE CHECK LIST OF REFERENCE VARIETIES.')
      ERRLEV=2
      GOTO 999
999   CONTINUE
      DEALLOCATE(OLDORD)
      RETURN
      END
C
C******************************************************************************
      SUBROUTINE MAFIT(K,XS,YS,GWK,MASPAN,MINSP)
C
C     Fits a moving average of length MASPAN to a dataset of K points
C   with the Y variable in Ys and the X variable in Xs.
C     At the ends of the series, averages of less than MASPAN points may
C   be used.  The minimum no of points in the average is MINSP.
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C*******************************************************************************
      DIMENSION XS(K),YS(K),GWK(K)
      IH=MASPAN/2 + 1
      KH=K-IH + 1
      IPT=0
      JPT=MINSP/2 + 1
      ISPAN=MINSP
C
10    FIT1=0.0
      FIT2=0.0
      WT1=0.0
      X=XS(JPT)
      FITX=YS(JPT)
      IX=(MASPAN-ISPAN)/2
      KX=1
         DO 30 I=1,ISPAN
         J=I+IX
         FIT1=FIT1+YS(IPT+I)
         WT1=WT1+1
30    CONTINUE
      J=IH
      IF(WT1.GT.0.0) FIT1=FIT1/WT1
      GWK(JPT)=FIT1
      JPT=JPT+1
      IF(ISPAN.LT.MASPAN .AND. JPT.LE.KH ) THEN
         ISPAN=ISPAN+2
      ELSE
         IPT=IPT+1
      ENDIF
      IF( JPT .LE. KH) GO TO 10
      IPT=IPT+1
      ISPAN=K-IPT
      IF(ISPAN.GE.MINSP) GO TO 10
C
      IF(MINSP.LE.1) GO TO 70
      IH=MINSP/2 + 1
      FIT1=GWK(IH)
      IH=IH-1
      FIT2=GWK(K-IH)
         DO 60 I=1,IH
               GWK(I)=FIT1
               GWK(K-I+1)=FIT2
   60 CONTINUE
70    RETURN
      END
C
C
C
C*******************************************************************
C
      SUBROUTINE FCPD1(Y,N,NV,NE,CRIT,RMV,MXITR,IRETC,VM,NEPV,
     1                   EE,NVPE,GMN,SS,IDF)
C
C       PROGRAM TO CALCULATE FOR A ROW-BY-COLUMN TABLE THE ROW AND
C           COLUMN MEANS ADJUSTED FOR MISSING VALUES WHERE NECESSARY .
C
C        VARIABLES -
C
C          Y - AN ARRAY OF SIZE N HOLDING NE X NV TABLE WHERE
C               NE IS NO. OF ROWS AND NV IS NO. OF COLUMNS.
C               TABLE IS STORED IN COLUMN ORDER WITHIN ROWS .
C       CRIT - ACCURACY CRITERION - SMALLEST CHANGE IN ADJUSTED
C                 MEANS BEFORE ITERATIONS ARE TERMINATED
C        RMV - VALUE GIVEN TO MISSING OBSERVATIONS
C      MXITR - MAXIMUM NO. OF ITERATIONS TO BE ALLOWED
C      IRETC - OUTPUT NO. OF ITERATIONS COMPLETED
C         VM - OUTPUT COLUMN MEANS
C       NEPV - OUTPUT COLUMN FREQUENCIES
C         EE - OUTPUT ROW MEANS
C       NVPE - OUTPUT ROW FREQUENCIES
C        GMN - GRAND MEAN
C         SS - MEAN SQUARES FROM ANALYSIS OF VARIANCE
C        IDF - DEGREES OF FREEDOM FROM ANALYSIS OF VARIANCE
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C*******************************************************************
      DIMENSION Y(N),VM(NV),NEPV(NV),EE(NE),NVPE(NE),SS(4),IDF(4)
      IRETC=0
      CRIT1=CRIT
C
C     +++++ Calculate unadjusted VTY means
C
      DO 60 I=1,NV
      VMT=0.0
      NTT=0
      DO 40 J=1,NE
        IJ=(J-1)*NV+I
        IF(Y(IJ).LE.RMV)GOTO 40
        NTT=NTT+1
        VMT=VMT+Y(IJ)
40    CONTINUE
      NEPV(I)=NTT
      IF(NTT.LE.0) THEN
        GOTO 50
      ELSE
        GOTO 55
      ENDIF
50    NTT=1
55    CONTINUE
      VM(I)=VMT/NTT
   60 CONTINUE
C
C     +++++ Calculate no. VTY/ENV
C
      DO 90 J=1,NE
      NTT=0
      DO 80 I=1,NV
      IJ=(J-1)*NV+I
      IF((Y(IJ)-RMV).LT.0.0) GOTO 70
      IF((Y(IJ)-RMV).EQ.0.0) GOTO 80
      IF((Y(IJ)-RMV).GT.0.0) GOTO 70
70    NTT=NTT+1
80    CONTINUE
      NVPE(J)=NTT
      EE(J)=0.0
   90 CONTINUE
C
C     +++++ If CRIT not set calculate default
C
      IF(CRIT.LE.0.0) THEN
        GOTO 100
      ELSE
        GOTO 120
      ENDIF
100   VMT=0.0
      NTT=0
      DO 110 I=1,NV
      VMT=VMT+VM(I)*NEPV(I)
      NTT=NTT+NEPV(I)
  110 CONTINUE
      IF(NTT.GT.0) CRIT1=(VMT/NTT)*0.001
C
C      +++++ Start of iterations
C
120   IRETC=IRETC+1
C
C     +++++ Calculate adjusted ENV effects
C
      EMT=0.0
      NTT=0
      DO 160 J=1,NE
      EE(J)=0
      IF(NVPE(J).LE.0) GOTO 160
      IF(NVPE(J).GT.0) GOTO 130
130   VMT=0.0
      DO 150 I=1,NV
      IJ=(J-1)*NV+I
      IF((Y(IJ)-RMV).EQ.0) GOTO 150
      IF((Y(IJ)-RMV).NE.0) GOTO 140
140   VMT=VMT+Y(IJ)-VM(I)
150   CONTINUE
      EE(J)=VMT/NVPE(J)
      EMT=EMT+EE(J)
      NTT=NTT+1
160   CONTINUE
C
C      +++++ Scale ENV effects to zero mean
C
      IF(NTT.GT.0) EMT=EMT/NTT
      DO 168 J=1,NE
      IF(NVPE(J).GT.0) EE(J)=EE(J)-EMT
      IF(NVPE(J).EQ.0) EE(J)=RMV
168   CONTINUE
C
C      +++++ Calculate adjusted variety means
C
      NVX=0
      DO 220 I=1,NV
      VM(I)=RMV
      IF(NEPV(I).LE.0) GOTO 220
      IF(NEPV(I).GT.0) GOTO 170
170   VMT=0.0
      DO 190 J=1,NE
        IJ=(J-1)*NV+I
        IF((Y(IJ)-RMV).EQ.0.0) GOTO 190
        IF((Y(IJ)-RMV).NE.0.0) GOTO 180
180     VMT=VMT+Y(IJ)-EE(J)
190   CONTINUE
      VMT=VMT/NEPV(I)
      IF((ABS(VMT-VM(I))-CRIT1).LE.0.0) GOTO 210
      IF((ABS(VMT-VM(I))-CRIT1).GT.0.0) GOTO 200
200   NVX=NVX+1
210   VM(I)=VMT
220   CONTINUE
C
C     +++++ Test converged
C
      IF(NVX.LE.0) GOTO 250
      IF(NVX.GT.0) GOTO 230
C
C     +++++ Test max. no. of iterations exceeded
C
230   IF((IRETC-MXITR).LT.0) THEN
        GOTO 120
      ELSE
        GOTO 240
      ENDIF
240   IRETC=-1
250   CONTINUE
C
C     +++++ Do analysis of variance
C
      DO 310 L=1,4
        IDF(L)=-1
        SS(L)=0.
310   CONTINUE
      GMN=0.
      DO 320 I=1,N
        IF(Y(I).GT.RMV)THEN
          GMN=GMN+Y(I)
          IDF(4)=IDF(4)+1
        ENDIF
320   CONTINUE
      IF(IDF(4).GT.0) GMN=GMN/(IDF(4)+1)
      M=0
      DO 328 J=1,NE
      KNT=0
      BJ=0.
      DO 323 K=1,NV
       M=M+1
       IF(Y(M).GT.RMV) THEN
         KNT=KNT+1
         BJ=BJ+Y(M)
       ENDIF
323   CONTINUE
      IF(KNT.EQ.0) GOTO 328
      BJ=BJ/KNT-GMN
      SS(1)=SS(1)+BJ*BJ*KNT
      IDF(1)=IDF(1)+1
328   CONTINUE
      M=0
      DO 340 J=1,NE
        DO 330 K=1,NV
          M=M+1
          IF(Y(M).GT.RMV) THEN
            SS(4)=SS(4)+(Y(M)-GMN)*(Y(M)-GMN)
            D=Y(M)-VM(K)-EE(J)
            SS(3)=SS(3)+D*D
          ENDIF
330     CONTINUE
340   CONTINUE
      DO 350 K=1,NV
        IF(NEPV(K).GT.0)IDF(2)=IDF(2)+1
350   CONTINUE
      IDF(3)=IDF(4)-IDF(1)-IDF(2)
      SS(2)=SS(4)-SS(1)-SS(3)
      DO 360 L=1,4
        IF(IDF(L).GT.0) SS(L)=SS(L)/IDF(L)
360   CONTINUE
      RETURN
      END
C
C
C******************************************************************
C
C
      SUBROUTINE MEAN1(Y,N,NV,NE,RMV,VM,NVM,EM,NEM,GMN,KNT)
C
C       CALCULATE SIMPLE MEANS FOR ROW-BY-COLUMN TABLE
C
C        VARIABLES -
C
C          Y - AN ARRAY OF SIZE N HOLDING NE X NV TABLE WHERE
C               NE IS NO. OF ROWS AND NV IS NO. OF COLUMNS .
C                DATA ARE STORED IN COLUMN ORDER WITHIN ROWS .
C        RMV - VALUE GIVEN TO MISSING OBSERVATIONS
C         VM - OUTPUT COLUMN MEANS
C        NVM - OUTPUT COLUMN FREQUENCIES
C         EM - OUTPUT ROW MEANS
C        NEM - OUTPUT ROW FREQUENCIES
C        GMN - GRAND MEAN
C        KNT - NO. OF NON-MISSING OBSERVATIONS
C
C******************************************************************
      DIMENSION Y(N),VM(NV),NVM(NV),EM(NE),NEM(NE)
      GMN=0
      KNT=0
      DO 20 J=1,NE
        EM(J)=0
        NEM(J)=0
        DO 10 I=1,NV
          K=(J-1)*NV+I
          IF(Y(K).GT.RMV) THEN
            EM(J)=EM(J)+Y(K)
            NEM(J)=NEM(J)+1
          ENDIF
10      CONTINUE
        IF(NEM(J).GT.0) THEN
          GMN=GMN+EM(J)
          KNT=KNT+NEM(J)
          EM(J)=EM(J)/NEM(J)
        ENDIF
        IF(NEM(J).EQ.0) EM(J)=RMV
20    CONTINUE
      IF(KNT.GT.0) GMN=GMN/KNT
      DO 40 I=1,NV
        VM(I)=0
        NVM(I)=0
        DO 30 J=1,NE
          K=(J-1)*NV+I
          IF(Y(K).GT.RMV) THEN
            VM(I)=VM(I)+Y(K)
            NVM(I)=NVM(I)+1
          ENDIF
30      CONTINUE
        IF(NVM(I).GT.0) VM(I)=VM(I)/NVM(I)
        IF(NVM(I).EQ.0) VM(I)=RMV
40    CONTINUE
      RETURN
      END
C
C
C*******************************************************************
C
C
      SUBROUTINE SORT(X,N)
C
C       ORDER THE N VALUES IN X INTO ASCENDING ORDER
C
C*******************************************************************
      DIMENSION X(N)
      I=1
1     I=I+I
      IF(I.LE.N) GOTO 1
      M=I-1
2     M=M/2
      IF(M.EQ.0) RETURN
      K=N-M
      DO 4 J=1,K
        KK=J
3       IF(KK.LT.1) GOTO 4
        IF(X(KK+M).GE.X(KK)) GOTO 4
        W=X(KK+M)
        X(KK+M)=X(KK)
        X(KK)=W
        KK=KK-M
        GOTO 3
4     CONTINUE
      GOTO 2
      END
C*******************************************************************
C
      SUBROUTINE SORTR(N,X,IORD)
C
C       Orders the N values in X into ascending order, and the elements
C     of IORD into the same order
C
C*******************************************************************
      DIMENSION X(N),IORD(N)
      I=1
1     I=I+I
      IF(I.LE.N) GOTO 1
      M=I-1
2     M=M/2
      IF(M.EQ.0) RETURN
      K=N-M
      DO 4 J=1,K
        KK=J
3       IF(KK.LT.1) GOTO 4
        IF(X(KK+M).GE.X(KK)) GOTO 4
        W=X(KK+M)
        IO=IORD(KK+M)
        X(KK+M)=X(KK)
        IORD(KK+M)=IORD(KK)
        X(KK)=W
        IORD(KK)=IO
        KK=KK-M
        GOTO 3
4     CONTINUE
      GOTO 2
      END
C*******************************************************************
C
C
      SUBROUTINE TABT(V,RESLT,IDF,EPSLN)
C
C       CALCULATE STUDENTS T GIVEN PROBABILITY AND DEGREES OF FREEDOM
C
C    VARIABLES:-
C         V - THE PROBABILITY LEVEL FOR WHICH TWO-TAILED T IS REQUIRED
C     RESLT - THE RESULTING T VALUE
C       IDF - DEGREES OF FREEDOM
C     EPSLN - THE REQUIRED ACCURACY
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C*******************************************************************
      IF (IDF.LE.0) GOTO 20
      IF (IDF.GT.0) GOTO 25
20    RESLT=10.0**10
      RETURN
25    Y=0.
5     X=Y
      Y=Y+1.
      CALL PROBT(PROB,Y,IDF)
      IF((2.*(1.-PROB)-V).LT.0.0) GOTO 3
      IF((2.*(1.-PROB)-V).GE.0.0) GOTO 5
3     R=(X+Y)/2.
      CALL PROBT(PROB,R,IDF)
      IF((2.*(1.-PROB)-V).LT.0.0) GOTO 1
      IF((2.*(1.-PROB)-V).GE.0.0) GOTO 11
11    X=R
      GO TO 2
1     Y=R
2     IF((Y-X-EPSLN).LT.0.0) GOTO 12
      IF((Y-X-EPSLN).GE.0.0) GOTO 3
12    RESLT=(X+Y)/2.
      RETURN
      END
C
C
C*******************************************************************
C
C
      SUBROUTINE PROBT(PROB,T,IDF)
C
C     CALCULATES THE AREA UP TO t UNDER A STUDENTS t DISTRIBUTION
C
C     VARIABLES:-
C          PROB - THE RESULTING AREA
C             T - THE INPUT VALUE OF T
C           IDF - DEGREES OF FREEDOM
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C*******************************************************************
      DATA G1/0.318309886/
      F=IDF
      A=T/SQRT(F)
      B=F/(F+T**2)
      IM2=IDF-2
      IOE=IDF-2*(IDF/2)
      S=1.
      C=1.
      KS=2+IOE
      FK=KS
      IF((IM2-2).LT.0) GOTO 6
      IF((IM2-2).GE.0) GOTO 7
7     DO 8 K=KS,IM2,2
        C=C*B*(FK-1.)/FK
        S=S+C
        FK=FK+2.
    8 CONTINUE
6     IF(IOE.LE.0) GOTO 1
      IF(IOE.GT.0) GOTO 2
1     PROB =0.5+0.5*A*SQRT(B)*S
      GO TO 3
2     IF((IDF-1).LE.0) THEN
        GOTO 4
      ELSE
        GOTO 5
      ENDIF
4     S=0.
5     PROB =0.5+(A*B*S+ATAN(A))*G1
3     RETURN
      END
C
C
C*******************************************************************
C
C
      SUBROUTINE RANK(X,N,IND,RMV,IMV,IX)
C
C       FORM RANK OF VALUES IN IX
C
C     VARIABLES:-
C          X - ARRAY OF SIZE N CONTAINING THE VALUES TO BE RANKED
C          N - THE NUMBER OF VALUES TO BE RANKED
C        IND - INTEGER INDICATING IF ASCENDING OR DESCENDING RANKS REQUIRED
C              1 - ASCENDING
C              2 - DESCENDING
C        RMV - THE VALUE GIVEN TO MISSING VALUES
C        IMV - INTEGER VALUE RETURNED FOR MISSING VALUES
C         IX - INTEGER ARRAY OF SIZE N CONTAINING THE RANKS
C
C*******************************************************************
      DIMENSION X(N),IX(N)
      INTEGER STEP
      IF(IND.NE.1) THEN
C
C   +++++ FOR DESCENDING RANKS
C
        N1=1
        N2=N
        STEP=1
      ELSE
C
C   +++++ FOR ASCENDING RANKS
C
        N1=N
        N2=1
        STEP=-1
      ENDIF
C
C
C   +++++ ZERO IX
C
C
      DO 3 I=1,N
           IX(I)=0
    3 CONTINUE
C
C   +++++ SET SMALL TO THE POSITION OF THE SMALLEST VALUE AND
C   +++++ KOUNT TO THAT OF THE LARGEST VALUE
C
      NMV=0
      ISMALL=1
      KOUNT=1
      DO 4 I=1,N
        IF(X(I).EQ.RMV) NMV=NMV+1
        IF(X(I).GT.X(KOUNT).AND.X(I).NE.RMV)KOUNT=I
        IF(X(I).LT.X(ISMALL).AND.X(I).GT.RMV)ISMALL=I
4     CONTINUE
C
C   +++++ CALCULATE THE RANK
C
      IF(IND.EQ.1) N1=N1-NMV
      IF(IND.EQ.2) N2=N2-NMV
      DO 1 I=N1,N2,STEP
        IDUM=ISMALL
        DO 2 J=1,N
          IF(X(J).LE.X(KOUNT).AND.IX(J).EQ.0.AND.
     1    X(J).GE.X(IDUM).AND.X(J).GT.RMV)IDUM=J
2       CONTINUE
        KOUNT=IDUM
        IX(KOUNT)=I
1     CONTINUE
C
C   +++++ SET THE RANK OF THE MISSING VALUES
C
      DO 5 I=1,N
           IF(X(I).EQ.RMV)IX(I)=IMV
    5 CONTINUE
      RETURN
      END
C*******************************************************************
C
C
      SUBROUTINE STEMLF(X,N,RMV,RSCALE,IWIDTH,ATOM,NOUT)
C
C      PRINT STEM-AND-LEAF HISTOGRAM ON channel NOUT
C
C       X - DATA ARRAY HOLDING N OBSERVATIONS TO BE PLOTTED
C     RMV - MISSING VALUE INDICATOR
C  RSCALE - VARIES DEPTH OF DISPLAY (DEFAULT=1.0)
C  IWIDTH - WIDTH OF DISPLAY (DEFAULT=55)
C    ATOM - CONSTANT ADDED TO RANGE TO AVOID ZERO DIVISION
C                IF RANGE IS VERY SMALL
C
C*******************************************************************
      DIMENSION X(N)
      CHARACTER*2 M(4),LF(500),IA(20)
      DATA IA/'0','1','2','3','4','5','6','7','8','9',
     +'0','1','2','3','4','5','6','7','8','9'/
      DATA M/'-',' ',' ','+'/
      CALL SORT(X,N)
      II=0
11    II=II+1
      IF(II.EQ.N) RETURN
      IF(X(II).LE.RMV) GOTO 11
      R=ATOM+(X(N)-X(II))/RSCALE
      C=10.**(11-INT(ALOG10(R)+10))
      MM=MIN0(2,MAX0(INT(R*C/25.),0))
      K=3*MM+2-150/(N+50)
      IF((K-1)*(K-2)*(K-5).EQ.0) C=C*10
      MU=10
      IF(K*(K-4)*(K-8).EQ.0) MU=5
      IF((K-1)*(K-5)*(K-6).EQ.0) MU=20
      I=1
      IF(X(II).GE.0) I=2
      D=MU*(INT(X(II)*C/MU)+I-2)/10
1     DO 2 K=1,IWIDTH
           LF(K)=M(2)
    2 CONTINUE
      IF(I.EQ.2.OR.D.LE.0) GOTO 3
      I=2
      D=D-MU/10.0
3     J=0
4     J=J+1
      IX=INT(0.5+ABS(X(II)*C-10*INT(D)))
      IF((X(II)*C-10*D).GE.0.5+(MU-1)*(I-1)) GOTO 5
      IF(J.LE.IWIDTH) LF(J)=IA(1+IX)
      II=II+1
      IF(II.GT.N) GOTO 5
      GOTO 4
5     ID=MOD(IABS(INT(D)),100)
      K1=1+ID/10
      K2=1+ID-10*(K1-1)
      IF(J.LE.IWIDTH+1) GOTO 6
      LF(IWIDTH-2)=M(4)
      LF(IWIDTH-1)=IA(1+(J-IWIDTH+2)/10)
      LF(IWIDTH)=IA(J-IWIDTH+3-10*((J-IWIDTH+2)/10))
6     K=MIN0(IWIDTH,J)
      WRITE(NOUT,7) M(I),IA(K1),IA(K2),M(3),(LF(J),J=1,K)
7     FORMAT(132A1)
      IF(II.GT.N) RETURN
      D=D+MU/10.0
      GOTO 1
      END
C
C
C******************************************************************
C
      SUBROUTINE SCAT(X,Y,Z,NR,N,NP,IWD,IDP,NDIVX,NDIVY,ATOM,RMV,NOUT,
     1IX,IY,SX,UX,SY,UY)
C
C       PLOT A SCATTER DIAGRAM
C
C      VARIABLES:-
C
C           X - AN ARRAY OF SIZE NR STORING THE X VALUES TO BE PLOTTED
C           Y - AN ARRAY OF SIZE NR*NP WHICH CONTAINS THE Y VALUES
C                     FOR THE NP VARIABLES
C           Z - AN ARRAY OF SIZE NR*NP WHICH IS USED AS A WORK ARRAY
C          NR - THE NUMBER OF VALUES TO BE PLOTTED IN EACH GRAPH
C          NP - THE NUMBER OF Y VARIABLES TO BE PLOTTED
C           N - NR*NP
C           M - A WORKING ARRAY OF SIZE ND
C          IM - A CHARACTER ARRAY CONTAINING THE CHARACTERS USED FOR PRINTING
C         IWD - AN INTEGER WHICH CONTROLS THE WIDTH OF THE GRAPH
C         IDP - AN INTEGER WHICH CONTROLS THE LENGTH OF THE GRAPH
C       NDIVX - THE DESIRED NUMBER OF UNITS ON THE X AXIS
C       NDIVY -  "     "      "     "   "    "  "  Y   "
C        ATOM - AN ADJUSTMENT TO PREVENT DIVISION BY ZERO IN THE SCALING ROUTINE
C        NOUT - OUTPUT CHANNEL NUMBER
C       IY,IX - FLAG THE SETTING OF THE X AND Y SCALE
C                 0 - SCALE NEEDS SETTING
C                 1 - SCALE ALREADY SET
C       SX,SY - THE LOWER SCALE ON THE X AND Y AXES
C       UX,UY - THE UPPER SCALE ON THE X AND Y AXES
C           M -  A WORKING ARRAY TO HOLD DIAGRAM
C
C       AMENDED BY I.M. NEVISON ON 11 FEBRUARY 1997 TO REMOVE FEATURES NOW
C       OBSOLETE IN FORTRAN 90
C*******************************************************************
      DIMENSION X(NR),Y(N),Z(N),M(3600)
      CHARACTER IM(30),BLANK(50)
      DATA IM/' ','*','2','3','4','5','6','7','8','9','*','h','p','g',
     1'o','f','n','e','m','d','l','c','k','.','*','x','*','-','I','.'/
      DO 20 I=1,50
            BLANK(I)=IM(1)
   20 CONTINUE
      DO 1 I=1,3600
           M(I)=0
    1 CONTINUE
      DO 2 I=1,NR
           Z(I)=X(I)
    2 CONTINUE
      CALL SORT(Z,NR)
      IF(IX.EQ.0)THEN
        SX=Z(1)
        UX=Z(NR)
        DO 21 I=1,NR-1
          IF(SX.LE.RMV)SX=Z(1+I)
          IF(UX.LE.RMV)UX=Z(NR-I)
21      CONTINUE
        CALL XSCALE(SX,UX,IWD,NDIVX,ATOM)
      ENDIF
      DO 3 I=1,NR
        DO 33 J=1,NP
              Z(J+NP*(I-1))=Y(I+(NR*(J-1)))
   33 CONTINUE
    3 CONTINUE
      CALL SORT(Z,N)
      IF(IY.EQ.0)THEN
        SY=Z(1)
        UY=Z(N)
        DO 22 I=1,N-1
          IF(SY.LE.RMV)SY=Z(1+I)
          IF(UY.LE.RMV)UY=Z(N-I)
22      CONTINUE
        CALL XSCALE(SY,UY,IDP,NDIVY,ATOM)
      ENDIF
      IOX=MAX0(1,1+INT(0.5+(IWD-1)*SX/(SX-UX)))
      IOY=MAX0(1,1+INT(0.5+(IDP-1)*SY/(SY-UY)))
      IF(NP.GT.1) GOTO 5
      DO 4 I=1,NR
        IF((Y(I).LE.RMV).OR.(X(I).LE.RMV))GOTO 4
        IN=1+INT(0.5+(IDP-1)*(Y(I)-SY)/(UY-SY))+IDP*
     1  INT(0.5+(IWD-1)*(X(I)-SX)/(UX-SX))
        M(IN)=MIN0(10,M(IN)+1)
        IF(Y(I).LE.RMV)M(IN)=0
4     CONTINUE
      GOTO 7
5     DO 66 J=1,MIN0(8,NP)
        K=9+2*(9-J)
        DO 6 I=1,NR
          IF((Y(I+(NR*(J-1))).LE.RMV).OR.(X(I).LE.RMV)) GOTO 6
          IN=1+INT(0.5+(IDP-1)*(Y(I+(NR*(J-1)))-SY)/(UY-SY))+IDP*
     1    INT(0.5+(IWD-1)*(X(I)-SX)/(UX-SX))
          IF(M(IN).GT.K+1)M(IN)=10
          IF(M(IN).EQ.K)M(IN)=K+1
          IF(M(IN).EQ.0)M(IN)=K
          IF(J.EQ.2)M(IN)=29
    6 CONTINUE
   66 CONTINUE
7     DO 8 J=1,IDP
           IF(M(J+IDP*(IOX-1)).EQ.0)M(J+IDP*(IOX-1))=28
    8 CONTINUE
      DO 9 I=1,IWD
           IF(M(IOY+IDP*(I-1)).EQ.0)M(IOY+IDP*(I-1))=27
    9 CONTINUE
      WRITE(NOUT,10) UY,(IM(1+M(IDP*I)),I=1,IWD)
10    FORMAT(F9.2,160A1)
      DO 11 J=2,IDP-1
            WRITE(NOUT,12) (IM(1+M(1-J+IDP*I)),I=1,IWD)
   11 CONTINUE
12    FORMAT(9X,160A1)
      WRITE(NOUT,10) SY,(IM(1+M(1+IDP*(I-1))),I=1,IWD)
      WRITE(NOUT,*)SX,(BLANK(I),I=1,IWD-12),UX
      RETURN
      END
C
C
C*******************************************************************
C
C
      SUBROUTINE XSCALE(S,U,LEN,NDIV,ATOM)
C
C
C      CALCULATE SCALES FOR SCATTER PLOT
C
C Name of subroutine changed from SCALE to XSCALE since SCALE is an intrinsic
C function in Fortran 90   -  IMN 11 February 1997
C
C*******************************************************************
      DIMENSION UNIT(3),DIF(3)
      CELL=ATOM+(U-S)/FLOAT(NDIV)
      UNIT(1)=10.**(INT(99.+ALOG10(CELL))-99)
      UNIT(2)=2*UNIT(1)
      UNIT(3)=5*UNIT(1)
      DO 1 I=1,3
           DIF(I)=ABS(CELL-UNIT(I))
    1 CONTINUE
      K=1
      IF(DIF(2).LT.DIF(1)) K=2
      IF(DIF(2).LT.DIF(2).AND.DIF(3).LT.DIF(1))K=3
      S=UNIT(K)*(INT(99.+(S/UNIT(K)))-99)
      U=UNIT(K)*(99.-INT(99.-(U/UNIT(K))))
      NDIV=(U-S)/UNIT(K)
      LEN=1+NDIV*((LEN-1)/NDIV)
      RETURN
      END


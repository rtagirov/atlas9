C***  LINE TRANSITIONS  ------------------------------------------------
   20 CONTINUE

      READ (KARTE,21) LEVUP,LEVLOW,LINECA,KRUDI,CEY,CO1,CO2,CO3,CO4
   21 FORMAT(10X,A10,2X,2A10,2X,A3,A4,1X,4G7.0)
      IF (LINECA(1:1).EQ.'F') THEN
         READ (LINECA(3:10),'(F8.4)') AUPLOW
         ELSE
         READ (LINECA(1:10),'(E10.4)') AUPLOW
         ENDIF
      LEVSEQ=1
C***  FIND UPPER INDEX
      DO 22 J=1,N
      NUP=J
      IF (LEVEL(J).EQ.LEVUP ) GOTO 23
   22 CONTINUE
      print *,levup
      print *, 'UPPER LINE LEVEL NOT FOUND'
      STOP 'ERROR'
C***  FIND LOWER INDEX
   23 CONTINUE
      DO 24 J=1,N
      LOW=J
      IF (LEVEL(J) .EQ. LEVLOW ) GOTO 25
   24 CONTINUE
      print *,levlow
      print *,  'LOWER LINE LEVEL NOT FOUND'
      STOP 'ERROR'
   25 IF (NATOM .GT. 1) THEN
         IF (NOM(NUP) .NE. NOM(LOW)) THEN
            print *,  'LINE BETWEEN DIFFERENT ELEMENTS'
            STOP 'ERROR'
            ENDIF
         ENDIF
      IF (NCHARG(NUP) .NE. NCHARG(LOW)) THEN
            print *,  'LINE BETWEEN DIFFERENT IONIZATION STAGES'
            STOP 'ERROR'
            ENDIF
      IF (NUP.LE.LOW) THEN
            print *,  'LINE TRANSITION INDICES WRONG'
            STOP 'ERROR'
            ENDIF
      IF (LINECA(1:1).EQ.'F') THEN
         IF (LINECA(1:2).EQ.'FE') GFG=AUPLOW
         IF (LINECA(1:2).EQ.'FA') GFG=WEIGHT(LOW)*AUPLOW/WEIGHT(NUP)
         AUPLOW=GFG*0.66702*(ELEVEL(NUP)-ELEVEL(LOW))**2
         ENDIF
c Potsdam convention:
      IF (AUPLOW.lt.0.) then
         GFG=-AUPLOW
         AUPLOW=GFG*0.66702*(ELEVEL(NUP)-ELEVEL(LOW))**2
c         print *,' Potsdam-f: ',low,nup,Auplow
         ENDIF
      EINST(NUP,LOW) = AUPLOW
      KEYCOL(NUP,LOW)=CEY
      IF ((CEY(:3).EQ.'BFK') .OR. (CEY(:2).EQ.'BK')) THEN
            COCO(NUP,LOW,1)=CO1
            COCO(NUP,LOW,2)=CO2
            COCO(NUP,LOW,3)=CO3
            COCO(NUP,LOW,4)=CO4
      ENDIF
      IF ((CEY(:3).EQ.'NIT') .OR. (CEY(:3).EQ.'CAR')) THEN
            COCO(NUP,LOW,1)=CO1
            COCO(NUP,LOW,2)=0.
            COCO(NUP,LOW,3)=0.
            COCO(NUP,LOW,4)=0.
      ENDIF
C***  RUDIMENTAL TRANSITIONS ARE MARKED BY -2. IN THE TRANSPOSED
C***  MATRIX ELEMENT  EINST(LOW,NUP)
      IF (KRUDI.NE.'   ') EINST(LOW,NUP)=-2.
      GOTO 1
     
C***  CONTINUUM TRANSITIONS    -----------------------------------------
   30 DECODE (80,31,KARTE)  LEVLOW, SIGMA,ALPLOW,SLOW,AGLOW
   31 FORMAT (10X,A10,1X,3G10.0,1X,A8,1X)
C   31 FORMAT(10X,A10,2X,2A10,2X,A8,1X,A4,1X,A4)
         
         LEVSEQ=1

C***  FIND LOWER INDEX
      DO 34 J=1,N
      LOW=J
      IF (LEVEL(J) .EQ. LEVLOW ) GOTO 35
   34 CONTINUE
      print *,  'LOWER CONTINUUM LEVEL NOT FOUND'
      STOP 'ERROR'
   35 CONTINUE
C***  FIND UPPER INDEX

      LOWP=LOW+1
      DO 32   J=LOWP,N
      NUP=J

      IF (NCHARG(LOW)+1 .EQ. NCHARG(NUP)) GOTO 33
   32 CONTINUE
      print *,  'UPPER CONTINUUM LEVEL NOT FOUND'
      print *, ' error'
      STOP 'ERROR'
   33 IF (NATOM .GT. 1) THEN
         IF (NOM(NUP) .NE. NOM(LOW)) THEN
            print *,  'CONTINUUM BETWEEN DIFFERENT ELEMENTS'
            STOP 'ERROR'
            ENDIF
         ENDIF
      !***  IN CASE OF CONTINUUM CALCULATON THE PARAMETERS SIGMA,ALPLOW,SLOW, AGLOW
      !***  CAN BE MISUSED AS KEYWORDS FOR DEFINING THE TABLES FROM WHICH TO READ 
      !***  CROSS SECTIONS
      !**   number
      EINST(LOW,NUP)=SIGMA
      !*** number
      ALPHA(LOW)=ALPLOW
      !*** number
      SEXPO(LOW)=SLOW
      !*** string der Laenge 8 (E.G.'TABLE')
      AGAUNT(LOW)=AGLOW
      GOTO 1
     
C***  SUM OF TRANSITIONS TO UPPER LEVELS WHICH ARE ASSUMED TO BE IN LTE
   40 DECODE (80,FORMAT_LTESUM,KARTE) LEVLOW,IRANGE,ASUM,COEFF1,COEFF2
!   41 FORMAT (10X,A10,1X,A8,1X,G9.0,1X,F7.0,1X,F7.0)
      LEVSEQ=1
C***  FIND LOWER INDEX
      DO 42 J=1,N
      LOW=J
      IF (LEVEL(J) .EQ. LEVLOW) GOTO 43
   42 CONTINUE
      print *,  'LOWER LTESUM LEVEL NOT FOUND'
      STOP 'ERROR'
   43 CONTINUE
      ALTESUM(1,LOW)=ASUM
      ALTESUM(2,LOW)=COEFF1
      ALTESUM(3,LOW)=COEFF2
      ENCODE (8,44,ALTESUM(4,LOW)) IRANGE
   44 FORMAT (A8)
      GOTO 1
     
C***  END OF INPUT DATA REACHED  ---------------------------------------







C***  TRANSITIONS ARE CHECKED FOR COMPLETENESS
      DO 7 I=1,N
      DO 7 J=1,N
      IF (NOM(I) .NE. NOM(J)) GOTO 7
      IF (NCHARG(I) .NE. NCHARG(J)) GOTO 8
      IF (I.LE.J) GOTO 7





      GOTO 7
    8 IF (I.GE.J) GOTO 7
C***  CHARGES MUST DIFFER BY 1
      IF (NCHARG(I)+1 .NE. NCHARG(J)) GOTO 7
C***  UPPER LEVEL MUST BE GROUND STATE OF THAT ION
      IF (NCHARG(J) .EQ. NCHARG(J-1)) GOTO 7
      IF (EINST(I,J) .LT. .0 ) THEN
            print *,  'CONTINUUM TRANSITION MISSING'
            STOP 'ERROR'
            ENDIF
    7 CONTINUE




















C***  GENERATE VECTORS INDNUP, INDLOW: LEVEL INCICES OF THE LINES
      DO 94 IND=1,linnum
      INDNUP(IND)=0
      INDLOW(IND)=0
   94 CONTINUE
      IND=0
      DO 95 NUP=2,N
      DO 95 LOW=1,NUP-1
      IF ((NCHARG(LOW) .NE. NCHARG(NUP)) .OR. (NOM(LOW) .NE. NOM(NUP)))
     $   GOTO 95
      IND=IND+1
      INDNUP(IND)=NUP
      INDLOW(IND)=LOW
   95 CONTINUE
      LASTIND=IND
     
C***  ASSIGNMENT OF IONIZATION ENERGIES (INPUT VALUES OF GROUND STATE)
C***  TO ALL LEVELS OF THE CORRESPONDING ELEMENT
      DO 13 I=1,N
      IF (EION(I) .NE. 0.) THEN
         DO 14 J=1,N
         IF ((NOM(J) .EQ. NOM(I)) .AND. (NCHARG(J) .EQ. NCHARG(I)))
     $      EION(J)=EION(I)
   14    CONTINUE
         ENDIF
   13 CONTINUE
     
C***  IF MAIN QUANTUM NUMBER IS GIVEN, LEVEL ENERGIES ARE COMPUTED BY
C***  RYDBERG FORMULA
      J=0
      DO 4 J=1,N
      IF (MAINQN(J).LE.1) GOTO 4
      F=FLOAT(MAINQN(J))
      ELEVEL(J)=(one-one/F/F)*EION(J)
    4 CONTINUE

!     IF AGAUNT(LOW) == 'TABLE' READ CROSS SECTION FROM TABLE
      do j = 1, N

         if (AGAUNT(j) == 'TABLE') call rdcsarr(level, j, wavarr, sigarr, levnum, NFDIM)

      enddo

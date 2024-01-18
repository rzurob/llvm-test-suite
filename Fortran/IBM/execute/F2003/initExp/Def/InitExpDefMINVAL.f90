!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefMINVAL.f
!*
!*  DATE                       : Mar 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an tranformational intrinsic
!*
!*  - MINVAL
!*  (319347)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefMINVAL
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),  PARAMETER :: I11(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(1),  PARAMETER :: I12(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(1),  PARAMETER :: I13(-5:-1,1:5, 0:-1)=0
  LOGICAL(1),  PARAMETER :: M1 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,I=1,125)/), (/5,5,5/))
  INTEGER(1),  PARAMETER :: Case3(5,5)          =RESHAPE((/(I,I=1,125,5)/), (/5,5/))

  INTEGER(KIND(MINVAL(I11)))        :: TI11(3) =MINVAL(I11)
  INTEGER(KIND(MINVAL(I12, DIM=1))) :: TI12(3) =MINVAL(I12)
  INTEGER(KIND(MINVAL(I13, DIM=1))) :: TI13(3) =MINVAL(I13)
  INTEGER(KIND(MINVAL(I13, DIM=3))) :: TI14(3) =MINVAL(I11, MASK=M1)
  INTEGER(KIND(MINVAL(I13, DIM=1))) :: TI15(5,5) =MINVAL(I11, MASK=M1, DIM=1)

  INTEGER(2),  PARAMETER :: I21(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(2),  PARAMETER :: I22(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(2),  PARAMETER :: I23(-5:-1,1:5, 0:-1)=0
  LOGICAL(2),  PARAMETER :: M2 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MINVAL(I21)))        :: TI21(3) =MINVAL(I21)
  INTEGER(KIND(MINVAL(I22, DIM=1))) :: TI22(3) =MINVAL(I22)
  INTEGER(KIND(MINVAL(I23, DIM=1))) :: TI23(3) =MINVAL(I23)
  INTEGER(KIND(MINVAL(I23, DIM=3))) :: TI24(3) =MINVAL(I21, MASK=M1)
  INTEGER(KIND(MINVAL(I23, DIM=1))) :: TI25(5,5) =MINVAL(I21, MASK=M2, DIM=1)

  INTEGER(4),  PARAMETER :: I41(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(4),  PARAMETER :: I42(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(4),  PARAMETER :: I43(-5:-1,1:5, 0:-1)=0
  LOGICAL(4),  PARAMETER :: M4 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MINVAL(I41)))        :: TI41(3) =MINVAL(I41)
  INTEGER(KIND(MINVAL(I42, DIM=1))) :: TI42(3) =MINVAL(I42)
  INTEGER(KIND(MINVAL(I43, DIM=1))) :: TI43(3) =MINVAL(I43)
  INTEGER(KIND(MINVAL(I43, DIM=3))) :: TI44(3) =MINVAL(I41, MASK=M4)
  INTEGER(KIND(MINVAL(I43, DIM=1))) :: TI45(5,5) =MINVAL(I41, MASK=M4, DIM=1)

  INTEGER(8),  PARAMETER :: I81(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(8),  PARAMETER :: I82(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(8),  PARAMETER :: I83(-5:-1,1:5, 0:-1)=0
  LOGICAL(8),  PARAMETER :: I8 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MINVAL(I81)))        :: TI81(3) =MINVAL(I81)
  INTEGER(KIND(MINVAL(I82, DIM=1))) :: TI82(3) =MINVAL(I82)
  INTEGER(KIND(MINVAL(I83, DIM=1))) :: TI83(3) =MINVAL(I83)
  INTEGER(KIND(MINVAL(I83, DIM=3))) :: TI84(3) =MINVAL(I81, MASK=M1)
  INTEGER(KIND(MINVAL(I83, DIM=1))) :: TI85(5,5) =MINVAL(I81, MASK=M1, DIM=1)

  REAL(4),  PARAMETER :: R41(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(4),  PARAMETER :: R42(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(4),  PARAMETER :: R43(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MINVAL(R41)))        :: TR41(3) =MINVAL(R41)
  REAL(KIND(MINVAL(R42, DIM=1))) :: TR42(3) =MINVAL(R42)
  REAL(KIND(MINVAL(R43, DIM=1))) :: TR43(3) =MINVAL(R43)
  REAL(KIND(MINVAL(R43, DIM=3))) :: TR44(3) =MINVAL(R41, MASK=M4)
  REAL(KIND(MINVAL(R43, DIM=1))) :: TR45(5,5) =MINVAL(R41, MASK=M1, DIM=1)

  REAL(8),  PARAMETER :: R81(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(8),  PARAMETER :: R82(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(8),  PARAMETER :: R83(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MINVAL(R81)))        :: TR81(3) =MINVAL(R81)
  REAL(KIND(MINVAL(R82, DIM=1))) :: TR82(3) =MINVAL(R82)
  REAL(KIND(MINVAL(R83, DIM=1))) :: TR83(3) =MINVAL(R83)
  REAL(KIND(MINVAL(R83, DIM=3))) :: TR84(3) =MINVAL(R81, MASK=M2)
  REAL(KIND(MINVAL(R83, DIM=1))) :: TR85(5,5) =MINVAL(R81, MASK=M1, DIM=1)

  REAL(16),  PARAMETER :: R161(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(16),  PARAMETER :: R162(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(16),  PARAMETER :: R163(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MINVAL(R161)))        :: TR161(3) =MINVAL(R161)
  REAL(KIND(MINVAL(R162, DIM=1))) :: TR162(3) =MINVAL(R162)
  REAL(KIND(MINVAL(R163, DIM=1))) :: TR163(3) =MINVAL(R163)
  REAL(KIND(MINVAL(R163, DIM=3))) :: TR164(3) =MINVAL(R161,   MASK=M1)
  REAL(KIND(MINVAL(R163, DIM=1))) :: TR165(5,5) =MINVAL(R161, MASK=M1, DIM=1)

  CHARACTER,  PARAMETER :: C1(-5:-1,1:5, 0:4) =RESHAPE((/(CHAR(I),I=1,125)/), (/5,5,5/))
  CHARACTER,  PARAMETER :: C2(-5:-1,1:5, 0:4) =RESHAPE((/(CHAR(10),I=1,125)/), (/5,5,5/))
  CHARACTER,  PARAMETER :: C3(-5:-1,1:5, 0:-1)=0

  CHARACTER(KIND(MINVAL(C1)))        :: TC1(3) =MINVAL(C1)
  CHARACTER(KIND(MINVAL(C2, DIM=1))) :: TC2(3) =MINVAL(C2)
  CHARACTER(KIND(MINVAL(C3, DIM=1))) :: TC3(3) =MINVAL(C3)
  CHARACTER(KIND(MINVAL(C3, DIM=3))) :: TC4(3) =MINVAL(C1, MASK=M1)
  CHARACTER(KIND(MINVAL(C3, DIM=1))) :: TC5(5,5) =MINVAL(C1, MASK=M1, DIM=1)

  CHARACTER(16) TC(3)

  IF (KIND(TI11)   .NE.  1  )              STOP 10
  IF (ANY( TI11    .NE.  1 ))              STOP 11
  IF (KIND(TI12)   .NE.  1  )              STOP 12
  IF (ANY( TI12    .NE.  -1 ))             STOP 13
  IF (KIND(TI13)   .NE.  1  )              STOP 14
  IF (ANY( TI13    .NE.  127 ))            STOP 15
  IF (KIND(TI14)   .NE.  1  )              STOP 16
  IF (ANY( TI14    .NE.  1 ))              STOP 17
  IF (KIND(TI15)   .NE.  1  )              STOP 18
  IF (ANY( TI15    .NE.  Case3 ))          STOP 19

  IF (KIND(TI21)   .NE.  2  )              STOP 20
  IF (ANY( TI21    .NE.  1 ))              STOP 21
  IF (KIND(TI22)   .NE.  2  )              STOP 22
  IF (ANY( TI22    .NE.  -1 ))             STOP 23
  IF (KIND(TI23)   .NE.  2  )              STOP 24
  IF (ANY( TI23    .NE.  32767))           STOP 25
  IF (KIND(TI24)   .NE.  2  )              STOP 26
  IF (ANY( TI24    .NE.  1 ))              STOP 27
  IF (KIND(TI25)   .NE.  2  )              STOP 28
  IF (ANY( TI25    .NE.  Case3 ))          STOP 29

  IF (KIND(TI41)   .NE.  4  )              STOP 30
  IF (ANY( TI41    .NE.  1  ))             STOP 31
  IF (KIND(TI42)   .NE.  4  )              STOP 32
  IF (ANY( TI42    .NE.  -1 ))             STOP 33
  IF (KIND(TI43)   .NE.  4  )              STOP 34
  IF (ANY( TI43    .NE.  2147483647 ))     STOP 35
  IF (KIND(TI44)   .NE.  4  )              STOP 36
  IF (ANY( TI44    .NE.  1  ))             STOP 37
  IF (KIND(TI45)   .NE.  4  )              STOP 38
  IF (ANY( TI45    .NE.  Case3 ))          STOP 39

  IF (KIND(TI81)   .NE.  8  )              STOP 40
  IF (ANY( TI81    .NE.  1  ))             STOP 41
  IF (KIND(TI82)   .NE.  8  )              STOP 42
  IF (ANY( TI82    .NE.  -1 ))             STOP 43
  IF (KIND(TI83)   .NE.  8  )              STOP 44
  IF (ANY( TI83    .NE.  9223372036854775807_8 )) STOP 45
  IF (KIND(TI84)   .NE.  8  )              STOP 46
  IF (ANY( TI84    .NE.  1 ))              STOP 47
  IF (KIND(TI85)   .NE.  8  )              STOP 48
  IF (ANY( TI85    .NE.  Case3 ))          STOP 49

  IF (KIND(TR41)   .NE.  4  )              STOP 50
  IF (ANY( TR41    .NE.  1 ))              STOP 51
  IF (KIND(TR42)   .NE.  4  )              STOP 52
  IF (ANY( TR42    .NE.  -1 ))             STOP 53
  IF (KIND(TR43)   .NE.  4  )              STOP 54
  WRITE(TC, FMT="(Z8.8)") TR43
  IF (ANY( TC(:)(1:8) .NE.  "7F800000" )) STOP 55
  IF (KIND(TR44)   .NE.  4  )              STOP 56
  IF (ANY( TR44    .NE.  1 ))              STOP 57
  IF (KIND(TR45)   .NE.  4  )              STOP 58
  IF (ANY( TR45    .NE.  Case3 ))          STOP 59

  IF (KIND(TR81)   .NE.  8  )              STOP 60
  IF (ANY( TR81    .NE.  1 ))              STOP 61
  IF (KIND(TR82)   .NE.  8  )              STOP 62
  IF (ANY( TR82    .NE.  -1 ))             STOP 63
  IF (KIND(TR83)   .NE.  8  )              STOP 64
  WRITE(TC, FMT="(Z16.16)") TR83
  IF (ANY( TC(:)(1:16) .NE.  "7FF0000000000000" )) STOP 65
  IF (KIND(TR84)   .NE.  8  )              STOP 66
  IF (ANY( TR84    .NE.  1  ))             STOP 67
  IF (KIND(TR85)   .NE.  8  )              STOP 68
  IF (ANY( TR85    .NE.  Case3 ))          STOP 69

  IF (KIND(TR161)   .NE.  16 )             STOP 70
  IF (ANY( TR161    .NE.  1 ))             STOP 71
  IF (KIND(TR162)   .NE.  16 )             STOP 72
  IF (ANY( TR162    .NE.  -1 ))            STOP 73
  IF (KIND(TR163)   .NE.  16 )             STOP 74
! IF (ANY( TR163    .NE.  0.17976931348623159077293051907890026Q+309 )) STOP 75
  IF (KIND(TR164)   .NE.  16 )             STOP 76
  IF (ANY( TR164    .NE.  1 ))             STOP 77
  IF (KIND(TR165)   .NE.  16 )             STOP 78
  IF (ANY( TR165    .NE.  Case3 ))         STOP 79

  IF (LEN(TC1)     .NE.  1  )              STOP 80
  IF (ANY( TC1     .NE.  CHAR(1) ))        STOP 81
  IF (LEN(TC2)     .NE.  1  )              STOP 82
  IF (ANY( TC2     .NE.  CHAR(10) ))       STOP 83
  IF (LEN(TC3)     .NE.  1  )              STOP 84
  IF (ANY( TC3     .NE.  CHAR(127) ))      STOP 85
  IF (LEN(TC4)     .NE.  1  )              STOP 86
  IF (ANY( TC4     .NE.  CHAR(1) ))        STOP 87
  IF (LEN(TC5)     .NE.  1  )              STOP 88
  IF (ANY( IACHAR(TC5)  .NE.  Case3 ))     STOP 89


  END




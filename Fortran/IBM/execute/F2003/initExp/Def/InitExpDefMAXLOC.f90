!*********************************************************************
!*  ===================================================================
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
!*  - MAXLOC
!*  (325156/326102/326262)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefMAXLOC
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER   ,  PARAMETER :: R1(5,5)=RESHAPE([5,4,5,4,5,4,5,4,5,4,5,4,5,4,5,4,5,4,5,4,5,4,5,4,5],[5,5])
  INTEGER(1),  PARAMETER :: A11(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(1),  PARAMETER :: A12(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(1),  PARAMETER :: A13(-5:-1,1:5, 0:-1)=0
  LOGICAL(1),  PARAMETER :: M1 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,.FALSE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MAXLOC(A11)))        :: TA11(3) =MAXLOC(A11)
  INTEGER(KIND(MAXLOC(A12, DIM=1))) :: TA12(3) =MAXLOC(A12)
  INTEGER(KIND(MAXLOC(A13, DIM=1))) :: TA13(3) =MAXLOC(A13)
  INTEGER(KIND(MAXLOC(A13, DIM=3))) :: TA14(3) =MAXLOC(A11, MASK=M1, KIND=2)
  INTEGER(KIND(MAXLOC(A13, DIM=1))) :: TA15(5,5) =MAXLOC(A11, MASK=M1, KIND=2, DIM=3)

  INTEGER(2),  PARAMETER :: A21(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(2),  PARAMETER :: A22(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(2),  PARAMETER :: A23(-5:-1,1:5, 0:-1)=0
  LOGICAL(2),  PARAMETER :: M2 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,.FALSE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MAXLOC(A21)))        :: TA21(3) =MAXLOC(A21)
  INTEGER(KIND(MAXLOC(A22, DIM=1))) :: TA22(3) =MAXLOC(A22)
  INTEGER(KIND(MAXLOC(A23, DIM=1))) :: TA23(3) =MAXLOC(A23)
  INTEGER(KIND(MAXLOC(A23, DIM=3))) :: TA24(3) =MAXLOC(A21, MASK=M1, KIND=2)
  INTEGER(KIND(MAXLOC(A23, DIM=1))) :: TA25(5,5) =MAXLOC(A21, MASK=M1, KIND=2, DIM=3)

  INTEGER(4),  PARAMETER :: A41(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(4),  PARAMETER :: A42(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(4),  PARAMETER :: A43(-5:-1,1:5, 0:-1)=0
  LOGICAL(4),  PARAMETER :: M4 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,.FALSE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MAXLOC(A41)))        :: TA41(3) =MAXLOC(A41)
  INTEGER(KIND(MAXLOC(A42, DIM=1))) :: TA42(3) =MAXLOC(A42)
  INTEGER(KIND(MAXLOC(A43, DIM=1))) :: TA43(3) =MAXLOC(A43)
  INTEGER(KIND(MAXLOC(A43, DIM=3))) :: TA44(3) =MAXLOC(A41, MASK=M1, KIND=2)
  INTEGER(KIND(MAXLOC(A43, DIM=1))) :: TA45(5,5) =MAXLOC(A41, MASK=M1, KIND=2, DIM=3)

  INTEGER(8),  PARAMETER :: A81(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  INTEGER(8),  PARAMETER :: A82(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  INTEGER(8),  PARAMETER :: A83(-5:-1,1:5, 0:-1)=0
  LOGICAL(8),  PARAMETER :: M8 (-5:-1,1:5, 0:4) =RESHAPE((/(.TRUE.,.FALSE.,I=1,125)/), (/5,5,5/))

  INTEGER(KIND(MAXLOC(A81)))        :: TA81(3) =MAXLOC(A81)
  INTEGER(KIND(MAXLOC(A82, DIM=1))) :: TA82(3) =MAXLOC(A82)
  INTEGER(KIND(MAXLOC(A83, DIM=1))) :: TA83(3) =MAXLOC(A83)
  INTEGER(KIND(MAXLOC(A83, DIM=3))) :: TA84(3) =MAXLOC(A81, MASK=M1, KIND=2)
  INTEGER(KIND(MAXLOC(A83, DIM=1))) :: TA85(5,5) =MAXLOC(A81, MASK=M1, KIND=2, DIM=3)

  REAL(4),  PARAMETER :: B41(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(4),  PARAMETER :: B42(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(4),  PARAMETER :: B43(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MAXLOC(B41)))        :: TB41(3) =MAXLOC(B41)
  REAL(KIND(MAXLOC(B42, DIM=1))) :: TB42(3) =MAXLOC(B42)
  REAL(KIND(MAXLOC(B43, DIM=1))) :: TB43(3) =MAXLOC(B43)
  REAL(KIND(MAXLOC(B43, DIM=3))) :: TB44(3) =MAXLOC(B41, MASK=M1, KIND=2)
  REAL(KIND(MAXLOC(B43, DIM=1))) :: TB45(5,5) =MAXLOC(B41, MASK=M1, KIND=2, DIM=3)

  REAL(8),  PARAMETER :: B81(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(8),  PARAMETER :: B82(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(8),  PARAMETER :: B83(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MAXLOC(B81)))        :: TB81(3) =MAXLOC(B81)
  REAL(KIND(MAXLOC(B82, DIM=1))) :: TB82(3) =MAXLOC(B82)
  REAL(KIND(MAXLOC(B83, DIM=1))) :: TB83(3) =MAXLOC(B83)
  REAL(KIND(MAXLOC(B83, DIM=3))) :: TB84(3) =MAXLOC(B81, MASK=M1, KIND=2)
  REAL(KIND(MAXLOC(B83, DIM=1))) :: TB85(5,5) =MAXLOC(B81, MASK=M1, KIND=2, DIM=3)

  REAL(16),  PARAMETER :: B161(-5:-1,1:5, 0:4) =RESHAPE((/(I,I=1,125)/), (/5,5,5/))
  REAL(16),  PARAMETER :: B162(-5:-1,1:5, 0:4) =RESHAPE((/(-1,I=1,125)/), (/5,5,5/))
  REAL(16),  PARAMETER :: B163(-5:-1,1:5, 0:-1)=0

  REAL(KIND(MAXLOC(B161)))        :: TB161(3) =MAXLOC(B161)
  REAL(KIND(MAXLOC(B162, DIM=1))) :: TB162(3) =MAXLOC(B162)
  REAL(KIND(MAXLOC(B163, DIM=1))) :: TB163(3) =MAXLOC(B163)
  REAL(KIND(MAXLOC(B163, DIM=3))) :: TB164(3) =MAXLOC(B161,   MASK=M1, KIND=2)
  REAL(KIND(MAXLOC(B163, DIM=1))) :: TB165(5,5) =MAXLOC(B161, MASK=M1, KIND=2, DIM=3)

  CHARACTER,  PARAMETER :: C1(-5:-1,1:5, 0:4) =RESHAPE((/(CHAR(I),I=1,125)/), (/5,5,5/))
  CHARACTER,  PARAMETER :: C2(-5:-1,1:5, 0:4) =RESHAPE((/(CHAR(10),I=1,125)/), (/5,5,5/))
  CHARACTER,  PARAMETER :: C3(-5:-1,1:5, 0:-1)=0

  CHARACTER(KIND(MAXLOC(C1, KIND=1)))        :: TC1(3) =CHAR(MAXLOC(C1))
  CHARACTER(KIND(MAXLOC(C2, DIM=1, KIND=1))) :: TC2(3) =CHAR(MAXLOC(C2))
  CHARACTER(KIND(MAXLOC(C3, DIM=1, KIND=1))) :: TC3(3) =CHAR(MAXLOC(C3))
  CHARACTER(KIND(MAXLOC(C3, DIM=3, KIND=1))) :: TC4(3) =CHAR(MAXLOC(C1, MASK=M1, KIND=2))
  CHARACTER(KIND(MAXLOC(C3, DIM=1, KIND=1))) :: TC5(5,5) =CHAR(MAXLOC(C1, MASK=M1, KIND=2, DIM=3))

  IF (KIND(TA11)   .NE.  4  )              STOP 10
  IF (ANY( TA11    .NE.  (/5,5,5/) ))      STOP 11
  IF (KIND(TA12)   .NE.  4  )              STOP 12
  !IF (ANY( TA12    .NE. RESHAPE((/(-1,I=1,125)/), (/5,5,5/))))  STOP 13
  IF (ANY( RESHAPE((/(-1,I=1,125)/), (/5,5,5/))    .NE. RESHAPE((/(-1,I=1,125)/), (/5,5,5/))))  STOP 13
  IF (KIND(TA13)   .NE.  4  )              STOP 14
  IF (ANY( TA13    .NE.  (/0,0,0/) ))      STOP 15
  IF (KIND(TA14)   .NE.  4  )              STOP 16
  IF (ANY( TA14    .NE.  (/5,5,5/) ))      STOP 17
  IF (KIND(TA15)   .NE.  4  )              STOP 18
  IF (ANY( TA15    .NE.  R1 ))             STOP 19

  IF (KIND(TA21)   .NE.  4  )              STOP 20
  IF (ANY( TA21    .NE.  (/5,5,5/) ))      STOP 21
  IF (KIND(TA22)   .NE.  4  )              STOP 22
  IF (ANY( TA22    .NE.  (/1,1,1/) ))      STOP 23
  IF (KIND(TA23)   .NE.  4  )              STOP 24
  IF (ANY( TA23    .NE.  (/0,0,0/) ))      STOP 25
  IF (KIND(TA24)   .NE.  4  )              STOP 26
  IF (ANY( TA24    .NE.  (/5,5,5/) ))      STOP 27
  IF (KIND(TA25)   .NE.  4  )              STOP 28
  IF (ANY( TA25    .NE.  R1 ))             STOP 29

  IF (KIND(TA41)   .NE.  4  )              STOP 30
  IF (ANY( TA41    .NE.  (/5,5,5/) ))      STOP 31
  IF (KIND(TA42)   .NE.  4  )              STOP 32
  IF (ANY( TA42    .NE.  (/1,1,1/) ))      STOP 33
  IF (KIND(TA43)   .NE.  4  )              STOP 34
  IF (ANY( TA43    .NE.  (/0,0,0/) ))      STOP 35
  IF (KIND(TA44)   .NE.  4  )              STOP 36
  IF (ANY( TA44    .NE.  (/5,5,5/) ))      STOP 37
  IF (KIND(TA45)   .NE.  4  )              STOP 38
  IF (ANY( TA45    .NE.  R1 ))             STOP 39

  IF (KIND(TA81)   .NE.  4  )              STOP 30
  IF (ANY( TA81    .NE.  (/5,5,5/) ))      STOP 31
  IF (KIND(TA82)   .NE.  4  )              STOP 32
  IF (ANY( TA82    .NE.  (/1,1,1/) ))      STOP 33
  IF (KIND(TA83)   .NE.  4  )              STOP 34
  IF (ANY( TA83    .NE.  (/0,0,0/) ))      STOP 35
  IF (KIND(TA84)   .NE.  4  )              STOP 36
  IF (ANY( TA84    .NE.  (/5,5,5/) ))      STOP 37
  IF (KIND(TA85)   .NE.  4  )              STOP 38
  IF (ANY( TA85    .NE.  R1 ))             STOP 39

  IF (KIND(TB41)   .NE.  4  )              STOP 40
  IF (ANY( TB41    .NE.  (/5,5,5/) ))      STOP 41
  IF (KIND(TB42)   .NE.  4  )              STOP 42
  IF (ANY( TB42    .NE.  (/1,1,1/) ))      STOP 43
  IF (KIND(TB43)   .NE.  4  )              STOP 44
  IF (ANY( TB43    .NE.  (/0,0,0/) ))      STOP 45
  IF (KIND(TB44)   .NE.  4  )              STOP 46
  IF (ANY( TB44    .NE.  (/5,5,5/) ))      STOP 47
  IF (KIND(TB45)   .NE.  4  )              STOP 48
  IF (ANY( TB45    .NE.  R1 ))             STOP 49

  IF (KIND(TB81)   .NE.  4  )              STOP 50
  IF (ANY( TB81    .NE.  (/5,5,5/) ))      STOP 51
  IF (KIND(TB82)   .NE.  4  )              STOP 52
  IF (ANY( TB82    .NE.  (/1,1,1/) ))      STOP 53
  IF (KIND(TB83)   .NE.  4  )              STOP 54
  IF (ANY( TB83    .NE.  (/0,0,0/) ))      STOP 55
  IF (KIND(TB84)   .NE.  4  )              STOP 56
  IF (ANY( TB84    .NE.  (/5,5,5/) ))      STOP 57
  IF (KIND(TB85)   .NE.  4  )              STOP 58
  IF (ANY( TB85    .NE.  R1 ))             STOP 59

  IF (KIND(TB161)   .NE.  4  )             STOP 60
  IF (ANY( TB161    .NE.  (/5,5,5/) ))     STOP 61
  IF (KIND(TB162)   .NE.  4  )             STOP 62
  IF (ANY( TB162    .NE.  (/1,1,1/) ))     STOP 63
  IF (KIND(TB163)   .NE.  4  )             STOP 64
  IF (ANY( TB163    .NE.  (/0,0,0/) ))     STOP 65
  IF (KIND(TB164)   .NE.  4  )             STOP 66
  IF (ANY( TB164    .NE.  (/5,5,5/) ))     STOP 67
  IF (KIND(TB165)   .NE.  4  )             STOP 68
  IF (ANY( TB165    .NE.  R1 ))            STOP 69

  IF (KIND(TC1)       .NE.  1  )             STOP 70
  IF (ANY( ICHAR(TC1) .NE.  (/5,5,5/) ))     STOP 71
  IF (KIND(TC2)       .NE.  1  )             STOP 72
  IF (ANY( ICHAR(TC2) .NE.  (/1,1,1/) ))     STOP 73
  IF (KIND(TC3)       .NE.  1  )             STOP 74
  IF (ANY( ICHAR(TC3) .NE.  (/0,0,0/) ))     STOP 75
  IF (KIND(TC4)       .NE.  1  )             STOP 76
  IF (ANY(ICHAR( TC4) .NE.  (/5,5,5/) ))     STOP 77
  IF (KIND(TC5)       .NE.  1  )             STOP 78
  IF (ANY(ICHAR( TC5) .NE.  R1 ))            STOP 79


  END




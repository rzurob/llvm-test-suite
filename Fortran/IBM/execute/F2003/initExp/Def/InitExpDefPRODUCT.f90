!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefPRODUCT.f
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
!*  - PRODUCT
!*  (319376)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  InitExpDefPRODUCT
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),   PARAMETER :: I11(2,1,2)   = 2
  LOGICAL(1),   PARAMETER :: M11(2,1,2)   = RESHAPE((/(.TRUE.,.FALSE., I=1,2)/),(/2,1,2/))
  INTEGER(1),   PARAMETER :: I12(2,1,2:1) = 2

  INTEGER(KIND(PRODUCT(I11)))             , PARAMETER :: TI11(2)  =PRODUCT(I11, .TRUE.)
  INTEGER(KIND(PRODUCT(TI11)))            , PARAMETER :: TI12(2)  =PRODUCT(I12)
  INTEGER(KIND(PRODUCT(TI12, M11(:,1,1)))), PARAMETER :: TI13(2,1)=PRODUCT(I11, 3)
  INTEGER(KIND(PRODUCT(TI13, M11(:,:,1)))), PARAMETER :: TI14(2,1)=PRODUCT(I11, MASK=M11, DIM=3)
  INTEGER                                             :: TI5(2,1)=RESHAPE((/4,1/),(/2,1/))
  INTEGER(2),   PARAMETER :: I21(2,1,2)   = 2
  LOGICAL(2),   PARAMETER :: M21(2,1,2)   = RESHAPE((/(.TRUE.,.FALSE., I=1,2)/),(/2,1,2/))
  INTEGER(2),   PARAMETER :: I22(2,1,2:1) = 2

  INTEGER(KIND(PRODUCT(I21))),              PARAMETER :: TI21(2)  =PRODUCT(I21, .TRUE.)
  INTEGER(KIND(PRODUCT(TI21))),             PARAMETER :: TI22(2)  =PRODUCT(I22)
  INTEGER(KIND(PRODUCT(TI22, M11(:,1,1)))), PARAMETER :: TI23(2,1)=PRODUCT(I21, 3)
  INTEGER(KIND(PRODUCT(TI23, M11(:,:,1)))), PARAMETER :: TI24(2,1)=PRODUCT(I21, MASK=M11, DIM=3)

  INTEGER(4),   PARAMETER :: I41(2,1,2)   = 2
  LOGICAL(4),   PARAMETER :: M41(2,1,2)   = RESHAPE((/(.TRUE.,.FALSE., I=1,2)/),(/2,1,2/))
  INTEGER(4),   PARAMETER :: I42(2,1,2:1) = 2

  INTEGER(KIND(PRODUCT(I41))),              PARAMETER :: TI41(2)  =PRODUCT(I41, .TRUE.)
  INTEGER(KIND(PRODUCT(TI41))),             PARAMETER :: TI42(2)  =PRODUCT(I42)
  INTEGER(KIND(PRODUCT(TI42, M21(:,1,1)))), PARAMETER :: TI43(2,1)=PRODUCT(I41, 3)
  INTEGER(KIND(PRODUCT(TI43, M11(:,:,1)))), PARAMETER :: TI44(2,1)=PRODUCT(I41, MASK=M21, DIM=3)

  INTEGER(8),   PARAMETER :: I81(2,1,2)   = 2
  LOGICAL(8),   PARAMETER :: M81(2,1,2)   = RESHAPE((/(.TRUE.,.FALSE., I=1,2)/),(/2,1,2/))
  INTEGER(8),   PARAMETER :: I82(2,1,2:1) = 2

  INTEGER(KIND(PRODUCT(I81))),              PARAMETER :: TI81(2)  =PRODUCT(I81, .TRUE.)
  INTEGER(KIND(PRODUCT(TI81))),             PARAMETER :: TI82(2)  =PRODUCT(I82)
  INTEGER(KIND(PRODUCT(TI82, M21(:,1,1)))), PARAMETER :: TI83(2,1)=PRODUCT(I81, 3)
  INTEGER(KIND(PRODUCT(TI83, M41(:,:,1)))), PARAMETER :: TI84(2,1)=PRODUCT(I81, MASK=M11, DIM=3)

  REAL(4),   PARAMETER :: R41(2,1,2)   = 2
  REAL(4),   PARAMETER :: R42(2,1,2:1) = 2

  REAL(KIND(PRODUCT(R41))),                 PARAMETER :: TR41(2)  =PRODUCT(R41, .TRUE.)
  REAL(KIND(PRODUCT(TR41))),                PARAMETER :: TR42(2)  =PRODUCT(R42)
  REAL(KIND(PRODUCT(TR42, M21(:,1,1)))),    PARAMETER :: TR43(2,1)=PRODUCT(R41, 3)
  REAL(KIND(PRODUCT(TR43, M11(:,:,1)))),    PARAMETER :: TR44(2,1)=PRODUCT(R41, MASK=M21, DIM=3)

  REAL(8),   PARAMETER :: R81(2,1,2)   = 2
  REAL(8),   PARAMETER :: R82(2,1,2:1) = 2

  REAL(KIND(PRODUCT(R81))),                 PARAMETER :: TR81(2)  =PRODUCT(R81, .TRUE.)
  REAL(KIND(PRODUCT(TR81))),                PARAMETER :: TR82(2)  =PRODUCT(R82)
  REAL(KIND(PRODUCT(TR82, M21(:,1,1)))),    PARAMETER :: TR83(2,1)=PRODUCT(R81, 3)
  REAL(KIND(PRODUCT(TR83, M11(:,:,1)))),    PARAMETER :: TR84(2,1)=PRODUCT(R81, MASK=M21, DIM=3)

  REAL(16),   PARAMETER :: R61(2,1,2)   = 2
  REAL(16),   PARAMETER :: R62(2,1,2:1) = 2

  REAL(KIND(PRODUCT(R61))),                 PARAMETER :: TR61(2)  =PRODUCT(R61, .TRUE.)
  REAL(KIND(PRODUCT(TR61))),                PARAMETER :: TR62(2)  =PRODUCT(R62)
  REAL(KIND(PRODUCT(TR62, M21(:,1,1)))),    PARAMETER :: TR63(2,1)=PRODUCT(R61, 3)
  REAL(KIND(PRODUCT(TR63, M81(:,:,1)))),    PARAMETER :: TR64(2,1)=PRODUCT(R61, MASK=M41, DIM=3)

  COMPLEX(4),   PARAMETER :: Z41(2,1,2)   = (1,1)
  COMPLEX(4),   PARAMETER :: Z42(2,1,2:1) = (1,1)

  COMPLEX(KIND(PRODUCT(Z41))),              PARAMETER :: TZ41(2)  =PRODUCT(Z41, .TRUE.)
  COMPLEX(KIND(PRODUCT(TZ41))),             PARAMETER :: TZ42(2)  =PRODUCT(Z42)
  COMPLEX(KIND(PRODUCT(TZ42, M21(:,1,1)))), PARAMETER :: TZ43(2,1)=PRODUCT(Z41, 3)
  COMPLEX(KIND(PRODUCT(TZ43, M11(:,:,1)))), PARAMETER :: TZ44(2,1)=PRODUCT(Z41, MASK=M21, DIM=3)
  COMPLEX(KIND(PRODUCT(TZ43, M11(:,:,1)))), PARAMETER :: TZ45(2,1)=PRODUCT(Z41, MASK=.FALSE., DIM=3)
  COMPLEX                                             :: TZ46(2,1)=RESHAPE((/(0.,2.),(1.,0.)/),(/2,1/))
  COMPLEX(8),   PARAMETER :: Z81(2,1,2)   = (1,1)
  COMPLEX(8),   PARAMETER :: Z82(2,1,2:1) = (1,1)

  COMPLEX(KIND(PRODUCT(Z81))) ,             PARAMETER :: TZ81(2)  =PRODUCT(Z81, .TRUE.)
  COMPLEX(KIND(PRODUCT(TZ81))),             PARAMETER :: TZ82(2)  =PRODUCT(Z82)
  COMPLEX(KIND(PRODUCT(TZ82, M21(:,1,1)))), PARAMETER :: TZ83(2,1)=PRODUCT(Z81, 3)
  COMPLEX(KIND(PRODUCT(TZ83, M11(:,:,1)))), PARAMETER :: TZ84(2,1)=PRODUCT(Z81, MASK=M21, DIM=3)
  COMPLEX(KIND(PRODUCT(TZ83, M11(:,:,1)))), PARAMETER :: TZ85(2,1)=PRODUCT(Z81, MASK=.FALSE., DIM=3)

  COMPLEX(16),   PARAMETER :: Z61(2,1,2)   = (1,1)
  COMPLEX(16),   PARAMETER :: Z62(2,1,2:1) = (1,1)

  COMPLEX(KIND(PRODUCT(Z61))),              PARAMETER :: TZ61(2)  =PRODUCT(Z61, .TRUE.)
  COMPLEX(KIND(PRODUCT(TZ61))),             PARAMETER :: TZ62(2)  =PRODUCT(Z62)
  COMPLEX(KIND(PRODUCT(TZ62, M21(:,1,1)))), PARAMETER :: TZ63(2,1)=PRODUCT(Z61, 3)
  COMPLEX(KIND(PRODUCT(TZ63, M11(:,:,1)))), PARAMETER :: TZ64(2,1)=PRODUCT(Z61, MASK=M21, DIM=3)
  COMPLEX(KIND(PRODUCT(TZ63, M11(:,:,1)))), PARAMETER :: TZ65(2,1)=PRODUCT(Z61, MASK=.FALSE., DIM=3)


  IF (KIND(TI11)   .NE.   1 )       STOP 11
  IF (ANY( TI11    .NE.   16))      STOP 12
  IF (KIND(TI12)   .NE.   1 )       STOP 13
  IF (ANY( TI12    .NE.   1 ))      STOP 14
  IF (KIND(TI13)   .NE.   1 )       STOP 15
  IF (ANY( TI13    .NE.   4 ))      STOP 16
  IF (KIND(TI14)   .NE.   1 )       STOP 17
  IF (ANY( TI14    .NE.   TI5))     STOP 18

  IF (KIND(TI21)   .NE.   2 )       STOP 21
  IF (ANY( TI21    .NE.   16))      STOP 22
  IF (KIND(TI22)   .NE.   2 )       STOP 23
  IF (ANY( TI22    .NE.   1 ))      STOP 24
  IF (KIND(TI23)   .NE.   2 )       STOP 25
  IF (ANY( TI23    .NE.   4 ))      STOP 26
  IF (KIND(TI24)   .NE.   2 )       STOP 27
  IF (ANY( TI24    .NE.   TI5 ))    STOP 28

  IF (KIND(TI41)   .NE.   4 )       STOP 31
  IF (ANY( TI41    .NE.   16))      STOP 32
  IF (KIND(TI42)   .NE.   4 )       STOP 33
  IF (ANY( TI42    .NE.   1 ))      STOP 34
  IF (KIND(TI43)   .NE.   4 )       STOP 35
  IF (ANY( TI43    .NE.   4 ))      STOP 36
  IF (KIND(TI44)   .NE.   4 )       STOP 37
  IF (ANY( TI44    .NE.   TI5 ))    STOP 38

  IF (KIND(TI81)   .NE.   8 )       STOP 41
  IF (ANY( TI81    .NE.   16))      STOP 42
  IF (KIND(TI82)   .NE.   8 )       STOP 43
  IF (ANY( TI82    .NE.   1 ))      STOP 44
  IF (KIND(TI83)   .NE.   8 )       STOP 45
  IF (ANY( TI83    .NE.   4 ))      STOP 46
  IF (KIND(TI84)   .NE.   8 )       STOP 47
  IF (ANY( TI84    .NE.   TI5 ))    STOP 48

  IF (KIND(TR41)   .NE.   4 )       STOP 51
  IF (ANY( TR41    .NE.   16))      STOP 52
  IF (KIND(TR42)   .NE.   4 )       STOP 53
  IF (ANY( TR42    .NE.   1 ))      STOP 54
  IF (KIND(TR43)   .NE.   4 )       STOP 55
  IF (ANY( TR43    .NE.   4 ))      STOP 56
  IF (KIND(TR44)   .NE.   4 )       STOP 57
  IF (ANY( TR44    .NE.   TI5 ))    STOP 58

  IF (KIND(TR81)   .NE.   8 )       STOP 61
  IF (ANY( TR81    .NE.   16))      STOP 62
  IF (KIND(TR82)   .NE.   8 )       STOP 63
  IF (ANY( TR82    .NE.   1 ))      STOP 64
  IF (KIND(TR83)   .NE.   8 )       STOP 65
  IF (ANY( TR83    .NE.   4 ))      STOP 66
  IF (KIND(TR84)   .NE.   8 )       STOP 67
  IF (ANY( TR84    .NE.   TI5 ))    STOP 68

  IF (KIND(TR61)   .NE.   16)       STOP 71
  IF (ANY( TR61    .NE.   16))      STOP 72
  IF (KIND(TR62)   .NE.   16)       STOP 73
  IF (ANY( TR62    .NE.   1 ))      STOP 74
  IF (KIND(TR63)   .NE.   16)       STOP 75
  IF (ANY( TR63    .NE.   4 ))      STOP 76
  IF (KIND(TR64)   .NE.   16)       STOP 77
  IF (ANY( TR64    .NE.   TI5 ))    STOP 78

  IF (KIND(TZ41)   .NE.   4 )       STOP 81
  IF (ANY( TZ41    .NE.   (-4,0)))  STOP 82
  IF (KIND(TZ42)   .NE.   4 )       STOP 83
  IF (ANY( TZ42    .NE.   (1,0)))   STOP 84
  IF (KIND(TZ43)   .NE.   4 )       STOP 85
  IF (ANY( TZ43    .NE.   (0,2) ))  STOP 86
  IF (KIND(TZ44)   .NE.   4 )       STOP 87
  IF (ANY( TZ44    .NE.   TZ46 ))   STOP 88
  IF (KIND(TZ45)   .NE.   4 )       STOP 89
  IF (ANY( TZ45    .NE.   (1,0) ))  STOP 80

  IF (KIND(TZ81)   .NE.   8 )       STOP 91
  IF (ANY( TZ81    .NE.   (-4,0)))  STOP 92
  IF (KIND(TZ82)   .NE.   8 )       STOP 93
  IF (ANY( TZ82    .NE.   (1,0)))   STOP 94
  IF (KIND(TZ83)   .NE.   8 )       STOP 95
  IF (ANY( TZ83    .NE.   (0,2) ))  STOP 96
  IF (KIND(TZ84)   .NE.   8 )       STOP 97
  IF (ANY( TZ84    .NE.   TZ46 ))   STOP 98
  IF (KIND(TZ85)   .NE.   8 )       STOP 99
  IF (ANY( TZ85    .NE.   (1,0) ))  STOP 90

  IF (KIND(TZ61)   .NE.   16 )      STOP 191
  IF (ANY( TZ61    .NE.   (-4,0)))  STOP 192
  IF (KIND(TZ62)   .NE.   16 )      STOP 193
  IF (ANY( TZ62    .NE.   (1,0)))   STOP 194
  IF (KIND(TZ63)   .NE.   16 )      STOP 195
  IF (ANY( TZ63    .NE.   (0,2) ))  STOP 196
  IF (KIND(TZ64)   .NE.   16 )      STOP 197
  IF (ANY( TZ64    .NE.   TZ46 ))   STOP 198
  IF (KIND(TZ65)   .NE.   16 )      STOP 199
  IF (ANY( TZ65    .NE.   (1,0) ))  STOP 190









  END




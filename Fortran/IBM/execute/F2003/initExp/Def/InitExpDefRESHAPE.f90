!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 30, 2006
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
!*  - RESHAPE
!*  (319105/320523)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT
    COMPLEX  :: Z=(0., 0.)
    LOGICAL(2) :: L(3,3)=.FALSE.
    PROCEDURE(), POINTER, NOPASS :: ProcPtr=>NULL()
    CONTAINS
    PROCEDURE  :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg
  END SUBROUTINE

  END MODULE


  PROGRAM  InitExpDefRESHAPE
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),   PARAMETER :: I1(8,8)   = RESHAPE((/(I, I=1,64)/),(/8,8/))
  INTEGER(1),   PARAMETER :: P11(17)   = RESHAPE((/(I, I=65,81)/),(/17/))
  INTEGER(1),   PARAMETER :: P12(17:0) = 0
  INTEGER(1),   PARAMETER :: O1(2)     = RESHAPE((/2,1/),(/2/))
  INTEGER(1),   PARAMETER :: R11(9,9)  = RESHAPE((/(I, I=1,81)/),(/9,9/))
  INTEGER(1),   PARAMETER :: R12(9,9)  = TRANSPOSE(R11)

  INTEGER(KIND(RESHAPE(I1,   (/8,8/))))         , PARAMETER :: TI11(8,8) = RESHAPE(R11, (/8,8/))
  INTEGER(KIND(RESHAPE(TI11, (/8,8/), P12)))    , PARAMETER :: TI12(8,8) = RESHAPE(R11, (/8,8/), P12)
  INTEGER(KIND(RESHAPE(TI12, (/8,8/), P11)))    , PARAMETER :: TI13(9,9) = RESHAPE(I1,  (/9,9/), P11)
  INTEGER(KIND(RESHAPE(I1,   (/8,8/), P11, O1))), PARAMETER :: TI14(9,9) = RESHAPE(I1,  (/9,9/), P11, O1)

  INTEGER(2),   PARAMETER :: I2(8,8)   = RESHAPE((/(I, I=1,64)/),(/8,8/))
  INTEGER(2),   PARAMETER :: P21(17)   = RESHAPE((/(I, I=65,81)/),(/17/))
  INTEGER(2),   PARAMETER :: P22(17:0) = 0
  INTEGER(2),   PARAMETER :: O2(2)     = RESHAPE((/2,1/),(/2/))
  INTEGER(2),   PARAMETER :: R21(9,9)  = RESHAPE((/(I, I=1,81)/),(/9,9/))
  INTEGER(2),   PARAMETER :: R22(9,9)  = TRANSPOSE(R11)

  INTEGER(KIND(RESHAPE(I2,   (/8,8/))))         , PARAMETER :: TI21(8,8) = RESHAPE(R21, (/8,8/))
  INTEGER(KIND(RESHAPE(TI21, (/8,8/), P22)))    , PARAMETER :: TI22(8,8) = RESHAPE(R21, (/8,8/), P22)
  INTEGER(KIND(RESHAPE(TI22, (/8,8/), P21)))    , PARAMETER :: TI23(9,9) = RESHAPE(I2,  (/9,9/), P21)
  INTEGER(KIND(RESHAPE(I2,   (/8,8/), P21, O1))), PARAMETER :: TI24(9,9) = RESHAPE(I2,  (/9,9/), P21, O1)

  INTEGER(4),   PARAMETER :: I4(8,8)   = RESHAPE((/(I, I=1,64)/),(/8,8/))
  INTEGER(4),   PARAMETER :: P41(17)   = RESHAPE((/(I, I=65,81)/),(/17/))
  INTEGER(4),   PARAMETER :: P42(17:0) = 0
  INTEGER(4),   PARAMETER :: O4(2)     = RESHAPE((/2,1/),(/2/))
  INTEGER(4),   PARAMETER :: R41(9,9)  = RESHAPE((/(I, I=1,81)/),(/9,9/))
  INTEGER(4),   PARAMETER :: R42(9,9)  = TRANSPOSE(R11)

  INTEGER(KIND(RESHAPE(I4,   (/8,8/))))         , PARAMETER :: TI41(8,8) = RESHAPE(R41, (/8,8/))
  INTEGER(KIND(RESHAPE(TI41, (/8,8/), P42)))    , PARAMETER :: TI42(8,8) = RESHAPE(R41, (/8,8/), P42)
  INTEGER(KIND(RESHAPE(TI42, (/8,8/), P41)))    , PARAMETER :: TI43(9,9) = RESHAPE(I4,  (/9,9/), P41)
  INTEGER(KIND(RESHAPE(I4,   (/8,8/), P41, O4))), PARAMETER :: TI44(9,9) = RESHAPE(I4,  (/9,9/), P41, O4)

  INTEGER(8),   PARAMETER :: I8(8,8)   = RESHAPE((/(I, I=1,64)/),(/8,8/))
  INTEGER(8),   PARAMETER :: P81(17)   = RESHAPE((/(I, I=65,81)/),(/17/))
  INTEGER(8),   PARAMETER :: P82(17:0) = 0
  INTEGER(8),   PARAMETER :: O8(2)     = RESHAPE((/2,1/),(/2/))
  INTEGER(8),   PARAMETER :: R81(9,9)  = RESHAPE((/(I, I=1,81)/),(/9,9/))
  INTEGER(8),   PARAMETER :: R82(9,9)  = TRANSPOSE(R11)

  INTEGER(KIND(RESHAPE(I8,   (/8,8/))))         , PARAMETER :: TI81(8,8) = RESHAPE(R81, (/8,8/))
  INTEGER(KIND(RESHAPE(TI81, (/8,8/), P82)))    , PARAMETER :: TI82(8,8) = RESHAPE(R81, (/8,8/), P82)
  INTEGER(KIND(RESHAPE(TI82, (/8,8/), P81)))    , PARAMETER :: TI83(9,9) = RESHAPE(I8,  (/9,9/), P81)
  INTEGER(KIND(RESHAPE(I8,   (/8,8/), P81, O4))), PARAMETER :: TI84(9,9) = RESHAPE(I8,  (/9,9/), P81, O8)

  COMPLEX(8),   PARAMETER :: Z8(8,8)    = RESHAPE((/((I,-I), I=1,64)/),(/8,8/))
  COMPLEX(8),   PARAMETER :: ZP81(17)   = RESHAPE((/((I,-I), I=65,81)/),(/17/))
  COMPLEX(8),   PARAMETER :: ZP82(17:0) = 0
  INTEGER(8),   PARAMETER :: ZO8(2)     = RESHAPE((/1,2/),(/2/))
  COMPLEX(8),   PARAMETER :: ZR81(9,9)  = RESHAPE((/((I,-I), I=1,81)/),(/9,9/))
  COMPLEX(8),   PARAMETER :: ZR82(9,9)  = TRANSPOSE(ZR81)

  COMPLEX(KIND(RESHAPE(Z8,   (/8,8/))))           , PARAMETER :: TZ81(8,8) = RESHAPE(ZR81, (/8,8/))
  COMPLEX(KIND(RESHAPE(TZ81, (/8,8/), ZP82)))     , PARAMETER :: TZ82(8,8) = RESHAPE(ZR81, (/8,8/), ZP82)
  COMPLEX(KIND(RESHAPE(TZ82, (/8,8/), ZP81)))     , PARAMETER :: TZ83(9,9) = RESHAPE(Z8,   (/9,9/), ZP81)
  COMPLEX(KIND(RESHAPE(Z8,   (/8,8/), ZP81, ZO8))), PARAMETER :: TZ84(9,9) = RESHAPE(Z8,   (/9,9/), ZP81, ZO8)

  TYPE(DT),   PARAMETER :: D(8,8)    = RESHAPE((/(DT((I,-I)), I=1,64)/),(/8,8/))
  TYPE(DT),   PARAMETER :: DP1(17)   = RESHAPE((/(DT((I,-I)), I=65,81)/),(/17/))
  TYPE(DT),   PARAMETER :: DP2(17:0) = DT()
  INTEGER,    PARAMETER :: DO(2)     = RESHAPE((/2,1/),(/2/))
  TYPE(DT),   PARAMETER :: DR1(9,9)  = RESHAPE((/(DT((I,-I)), I=1,81)/),(/9,9/))
  TYPE(DT),   PARAMETER :: DR2(9,9)  = TRANSPOSE(DR1)

  TYPE(DT)    , PARAMETER :: TD1(8,8) = RESHAPE(DR1, (/8,8/))
  TYPE(DT)    , PARAMETER :: TD2(8,8) = RESHAPE(DR1, (/8,8/), DP2)
  TYPE(DT)    , PARAMETER :: TD3(9,9) = RESHAPE(D,   (/9,9/), DP1)
  TYPE(DT)    , PARAMETER :: TD4(9,9) = RESHAPE(D,   (/9,9/), DP1, DO)



  IF (KIND(TI11)   .NE.   1 )       ERROR STOP 11
  IF (ANY( TI11    .NE.   I1))      ERROR STOP 12
  IF (KIND(TI12)   .NE.   1 )       ERROR STOP 13
  IF (ANY( TI12    .NE.   I1))      ERROR STOP 14
  IF (KIND(TI13)   .NE.   1 )       ERROR STOP 15
  IF (ANY( TI13    .NE.   R11))     ERROR STOP 16
  IF (KIND(TI14)   .NE.   1 )       ERROR STOP 17
  IF (ANY( TI14    .NE.   R12))     ERROR STOP 18

  IF (KIND(TI21)   .NE.   2 )       ERROR STOP 21
  IF (ANY( TI21    .NE.   I2))      ERROR STOP 22
  IF (KIND(TI22)   .NE.   2 )       ERROR STOP 23
  IF (ANY( TI22    .NE.   I2))      ERROR STOP 24
  IF (KIND(TI23)   .NE.   2 )       ERROR STOP 25
  IF (ANY( TI23    .NE.   R21))     ERROR STOP 26
  IF (KIND(TI24)   .NE.   2 )       ERROR STOP 27
  IF (ANY( TI24    .NE.   R22))     ERROR STOP 28

  IF (KIND(TI41)   .NE.   4 )       ERROR STOP 31
  IF (ANY( TI41    .NE.   I4))      ERROR STOP 32
  IF (KIND(TI42)   .NE.   4 )       ERROR STOP 33
  IF (ANY( TI42    .NE.   I4))      ERROR STOP 34
  IF (KIND(TI43)   .NE.   4 )       ERROR STOP 35
  IF (ANY( TI43    .NE.   R41))     ERROR STOP 36
  IF (KIND(TI44)   .NE.   4 )       ERROR STOP 37
  IF (ANY( TI44    .NE.   R42))     ERROR STOP 38

  IF (KIND(TI81)   .NE.   8 )       ERROR STOP 41
  IF (ANY( TI81    .NE.   I4))      ERROR STOP 42
  IF (KIND(TI82)   .NE.   8 )       ERROR STOP 43
  IF (ANY( TI82    .NE.   I4))      ERROR STOP 44
  IF (KIND(TI83)   .NE.   8 )       ERROR STOP 45
  IF (ANY( TI83    .NE.   R41))     ERROR STOP 46
  IF (KIND(TI84)   .NE.   8 )       ERROR STOP 47
  IF (ANY( TI84    .NE.   R42))     ERROR STOP 48

  IF (KIND(TZ81)   .NE.   8 )       ERROR STOP 51
  IF (ANY( TZ81    .NE.   Z8))      ERROR STOP 52
  IF (KIND(TZ82)   .NE.   8 )       ERROR STOP 53
  IF (ANY( TZ82    .NE.   Z8))      ERROR STOP 54
  IF (KIND(TZ83)   .NE.   8 )       ERROR STOP 55
  IF (ANY( TZ83    .NE.   ZR81))    ERROR STOP 56
  IF (KIND(TZ84)   .NE.   8 )       ERROR STOP 57
  IF (ANY( TZ84    .NE.   ZR81))    ERROR STOP 58

  IF (ANY( TD1%Z   .NE.   D%Z))       ERROR STOP 62
  IF (ANY( TD2%Z   .NE.   D%Z))       ERROR STOP 64
  IF (ANY( TD3%Z   .NE.   DR1%Z))     ERROR STOP 66
  IF (ANY( TD4%Z   .NE.   DR2%Z))     ERROR STOP 68


  END




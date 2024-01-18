!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefTRANSFER.f
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
!*  - TRANSFER
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefTRANSFER
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1), PARAMETER :: I11       = 1
  INTEGER(1), PARAMETER :: I12(2)    = 1

  INTEGER(KIND(TRANSFER(TRANSFER(I11, I12), I11))), PARAMETER       :: TI11    &
                   = TRANSFER(TRANSFER(I11, I12), I11)
  INTEGER(KIND(TRANSFER(TRANSFER(I12, I12), I12))), PARAMETER       :: TI12(2) &
                   = TRANSFER(TRANSFER(I12, I12), I12)
  INTEGER(KIND(TRANSFER(TRANSFER(I12, I11, 2), I12))), PARAMETER    :: TI13(2) &
                   = TRANSFER(TRANSFER(I12, I11, 2), I12)
  INTEGER(KIND(TRANSFER(TRANSFER(I12, I12, 2), I12, 2))), PARAMETER :: TI14(2) &
                   = TRANSFER(TRANSFER(I12, I12, 2), I12, 2)

  INTEGER(2), PARAMETER :: I21       = 1
  INTEGER(2), PARAMETER :: I22(2)    = 1

  INTEGER(KIND(TRANSFER(TRANSFER(I21, I22), I21))), PARAMETER       :: TI21    &
                   = TRANSFER(TRANSFER(I21, I22), I21)
  INTEGER(KIND(TRANSFER(TRANSFER(I22, I22), I22))), PARAMETER       :: TI22(2) &
                   = TRANSFER(TRANSFER(I22, I22), I22)
  INTEGER(KIND(TRANSFER(TRANSFER(I22, I21, 2), I22))), PARAMETER    :: TI23(2) &
                   = TRANSFER(TRANSFER(I22, I21, 2), I22)
  INTEGER(KIND(TRANSFER(TRANSFER(I22, I22, 2), I22, 2))), PARAMETER :: TI24(2) &
                   = TRANSFER(TRANSFER(I22, I22, 2), I22, 2)

  INTEGER(4), PARAMETER :: I41       = 1
  INTEGER(4), PARAMETER :: I42(2)    = 1

  INTEGER(KIND(TRANSFER(TRANSFER(I41, I42), I41))), PARAMETER       :: TI41    &
                   = TRANSFER(TRANSFER(I41, I42), I41)
  INTEGER(KIND(TRANSFER(TRANSFER(I42, I42), I42))), PARAMETER       :: TI42(2) &
                   = TRANSFER(TRANSFER(I42, I42), I42)
  INTEGER(KIND(TRANSFER(TRANSFER(I42, I41, 2), I42))), PARAMETER    :: TI43(2) &
                   = TRANSFER(TRANSFER(I42, I41, 2), I42)
  INTEGER(KIND(TRANSFER(TRANSFER(I42, I42, 2), I42, 2))), PARAMETER :: TI44(2) &
                   = TRANSFER(TRANSFER(I42, I42, 2), I42, 2)

  INTEGER(8), PARAMETER :: I81       = 1
  INTEGER(8), PARAMETER :: I82(2)    = 1

  INTEGER(KIND(TRANSFER(TRANSFER(I81, I82), I81))), PARAMETER       :: TI81    &
                   = TRANSFER(TRANSFER(I81, I82), I81)
  INTEGER(KIND(TRANSFER(TRANSFER(I82, I82), I82))), PARAMETER       :: TI82(2) &
                   = TRANSFER(TRANSFER(I82, I82), I82)
  INTEGER(KIND(TRANSFER(TRANSFER(I82, I81, 2), I82))), PARAMETER    :: TI83(2) &
                   = TRANSFER(TRANSFER(I82, I81, 2), I82)
  INTEGER(KIND(TRANSFER(TRANSFER(I82, I82, 2), I82, 2))), PARAMETER :: TI84(2) &
                   = TRANSFER(TRANSFER(I82, I82, 2), I82, 2)

  REAL(4), PARAMETER :: R41       = 1.1
  REAL(4), PARAMETER :: R42(2)    = 1.1

  REAL(KIND(TRANSFER(TRANSFER(R41, R42), R41))), PARAMETER       :: TR41    &
                   = TRANSFER(TRANSFER(R41, R42), R41)
  REAL(KIND(TRANSFER(TRANSFER(R42, R42), R42))), PARAMETER       :: TR42(2) &
                   = TRANSFER(TRANSFER(R42, R42), R42)
  REAL(KIND(TRANSFER(TRANSFER(R42, R41, 2), R42))), PARAMETER    :: TR43(2) &
                   = TRANSFER(TRANSFER(R42, R41, 2), R42)
  REAL(KIND(TRANSFER(TRANSFER(R42, R42, 2), R42, 2))), PARAMETER :: TR44(2) &
                   = TRANSFER(TRANSFER(R42, R42, 2), R42, 2)

  REAL(8), PARAMETER :: R81       = 1.1
  REAL(8), PARAMETER :: R82(2)    = 1.1

  REAL(KIND(TRANSFER(TRANSFER(R81, R82), R81))), PARAMETER       :: TR81    &
                   = TRANSFER(TRANSFER(R81, R82), R81)
  REAL(KIND(TRANSFER(TRANSFER(R82, R82), R82))), PARAMETER       :: TR82(2) &
                   = TRANSFER(TRANSFER(R82, R82), R82)
  REAL(KIND(TRANSFER(TRANSFER(R82, R81, 2), R82))), PARAMETER    :: TR83(2) &
                   = TRANSFER(TRANSFER(R82, R81, 2), R82)
  REAL(KIND(TRANSFER(TRANSFER(R82, R82, 2), R82, 2))), PARAMETER :: TR84(2) &
                   = TRANSFER(TRANSFER(R82, R82, 2), R82, 2)

  REAL(16), PARAMETER :: R61       = 1.1
  REAL(16), PARAMETER :: R62(2)    = 1.1

  REAL(KIND(TRANSFER(TRANSFER(R61, R62), R61))), PARAMETER       :: TR61    &
                   = TRANSFER(TRANSFER(R61, R62), R61)
  REAL(KIND(TRANSFER(TRANSFER(R62, R62), R62))), PARAMETER       :: TR62(2) &
                   = TRANSFER(TRANSFER(R62, R62), R62)
  REAL(KIND(TRANSFER(TRANSFER(R62, R61, 2), R62))), PARAMETER    :: TR63(2) &
                   = TRANSFER(TRANSFER(R62, R61, 2), R62)
  REAL(KIND(TRANSFER(TRANSFER(R62, R62, 2), R62, 2))), PARAMETER :: TR64(2) &
                   = TRANSFER(TRANSFER(R62, R62, 2), R62, 2)


  COMPLEX(4), PARAMETER :: Z41       = (1.1,1.1)
  COMPLEX(4), PARAMETER :: Z42(2)    = (1.1,1.1)

  COMPLEX(KIND(TRANSFER(TRANSFER(Z41, Z42), Z41))), PARAMETER       :: TZ41    &
                   = TRANSFER(TRANSFER(Z41, Z42), Z41)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z42, Z42), Z42))), PARAMETER       :: TZ42(2) &
                   = TRANSFER(TRANSFER(Z42, Z42), Z42)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z42, Z41, 2), Z42))), PARAMETER    :: TZ43(2) &
                   = TRANSFER(TRANSFER(Z42, Z41, 2), Z42)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z42, Z42, 2), Z42, 2))), PARAMETER :: TZ44(2) &
                   = TRANSFER(TRANSFER(Z42, Z42, 2), Z42, 2)

  COMPLEX(8), PARAMETER :: Z81       = (1.1,1.1)
  COMPLEX(8), PARAMETER :: Z82(2)    = (1.1,1.1)

  COMPLEX(KIND(TRANSFER(TRANSFER(Z81, Z82), Z81))), PARAMETER       :: TZ81    &
                   = TRANSFER(TRANSFER(Z81, Z82), Z81)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z82, Z82), Z82))), PARAMETER       :: TZ82(2) &
                   = TRANSFER(TRANSFER(Z82, Z82), Z82)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z82, Z81, 2), Z82))), PARAMETER    :: TZ83(2) &
                   = TRANSFER(TRANSFER(Z82, Z81, 2), Z82)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z82, Z82, 2), Z82, 2))), PARAMETER :: TZ84(2) &
                   = TRANSFER(TRANSFER(Z82, Z82, 2), Z82, 2)

  COMPLEX(16), PARAMETER :: Z61       = (1.1,1.1)
  COMPLEX(16), PARAMETER :: Z62(2)    = (1.1,1.1)

  COMPLEX(KIND(TRANSFER(TRANSFER(Z61, Z62), Z61))), PARAMETER       :: TZ61    &
                   = TRANSFER(TRANSFER(Z61, Z62), Z61)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z62, Z62), Z62))), PARAMETER       :: TZ62(2) &
                   = TRANSFER(TRANSFER(Z62, Z62), Z62)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z62, Z61, 2), Z62))), PARAMETER    :: TZ63(2) &
                   = TRANSFER(TRANSFER(Z62, Z61, 2), Z62)
  COMPLEX(KIND(TRANSFER(TRANSFER(Z62, Z62, 2), Z62, 2))), PARAMETER :: TZ64(2) &
                   = TRANSFER(TRANSFER(Z62, Z62, 2), Z62, 2)



  IF(  KIND(TI11)     .NE. 1 )       STOP 11
  IF(  TI11           .NE. I11 )     STOP 12
  IF(  KIND(TI12)     .NE. 1 )       STOP 13
  IF(  ANY(TI12       .NE. I12 ))    STOP 14
  IF(  KIND(TI13)     .NE. 1 )       STOP 15
  IF(  ANY(TI13       .NE. I12 ))    STOP 16
  IF(  KIND(TI14)     .NE. 1 )       STOP 17
  IF(  ANY(TI14       .NE. I12 ))    STOP 18

  IF(  KIND(TI21)     .NE. 2 )       STOP 21
  IF(  TI21           .NE. I21 )     STOP 22
  IF(  KIND(TI22)     .NE. 2 )       STOP 23
  IF(  ANY(TI22       .NE. I22 ))    STOP 24
  IF(  KIND(TI23)     .NE. 2 )       STOP 25
  IF(  ANY(TI23       .NE. I22 ))    STOP 26
  IF(  KIND(TI24)     .NE. 2 )       STOP 27
  IF(  ANY(TI24       .NE. I22 ))    STOP 28

  IF(  KIND(TI41)     .NE. 4 )       STOP 31
  IF(  TI41           .NE. I41 )     STOP 32
  IF(  KIND(TI42)     .NE. 4 )       STOP 33
  IF(  ANY(TI42       .NE. I42 ))    STOP 34
  IF(  KIND(TI43)     .NE. 4 )       STOP 35
  IF(  ANY(TI43       .NE. I42 ))    STOP 36
  IF(  KIND(TI44)     .NE. 4 )       STOP 37
  IF(  ANY(TI44       .NE. I42 ))    STOP 38

  IF(  KIND(TI81)     .NE. 8 )       STOP 41
  IF(  TI81           .NE. I81 )     STOP 42
  IF(  KIND(TI82)     .NE. 8 )       STOP 43
  IF(  ANY(TI82       .NE. I82 ))    STOP 44
  IF(  KIND(TI83)     .NE. 8 )       STOP 45
  IF(  ANY(TI83       .NE. I82 ))    STOP 46
  IF(  KIND(TI84)     .NE. 8 )       STOP 47
  IF(  ANY(TI84       .NE. I82 ))    STOP 48

  IF(  KIND(TR41)     .NE. 4 )       STOP 51
  IF(  TR41           .NE. R41 )     STOP 52
  IF(  KIND(TR42)     .NE. 4 )       STOP 53
  IF(  ANY(TR42       .NE. R42 ))    STOP 54
  IF(  KIND(TR43)     .NE. 4 )       STOP 55
  IF(  ANY(TR43       .NE. R42 ))    STOP 56
  IF(  KIND(TR44)     .NE. 4 )       STOP 57
  IF(  ANY(TR44       .NE. R42 ))    STOP 58

  IF(  KIND(TR81)     .NE. 8 )       STOP 61
  IF(  TR81           .NE. R81 )     STOP 62
  IF(  KIND(TR82)     .NE. 8 )       STOP 63
  IF(  ANY(TR82       .NE. R82 ))    STOP 64
  IF(  KIND(TR83)     .NE. 8 )       STOP 65
  IF(  ANY(TR83       .NE. R82 ))    STOP 66
  IF(  KIND(TR84)     .NE. 8 )       STOP 67
  IF(  ANY(TR84       .NE. R82 ))    STOP 68

  IF(  KIND(TR61)     .NE. 16 )      STOP 71
  IF(  TR61           .NE. R61 )     STOP 72
  IF(  KIND(TR62)     .NE. 16 )      STOP 73
  IF(  ANY(TR62       .NE. R62 ))    STOP 74
  IF(  KIND(TR63)     .NE. 16 )      STOP 75
  IF(  ANY(TR63       .NE. R62 ))    STOP 76
  IF(  KIND(TR64)     .NE. 16 )      STOP 77
  IF(  ANY(TR64       .NE. R62 ))    STOP 78

  IF(  KIND(TZ41)     .NE. 4 )       STOP 151
  IF(  TZ41           .NE. Z41 )     STOP 152
  IF(  KIND(TZ42)     .NE. 4 )       STOP 153
  IF(  ANY(TZ42       .NE. Z42 ))    STOP 154
  IF(  KIND(TZ43)     .NE. 4 )       STOP 155
  IF(  ANY(TZ43       .NE. Z42 ))    STOP 156
  IF(  KIND(TZ44)     .NE. 4 )       STOP 157
  IF(  ANY(TZ44       .NE. Z42 ))    STOP 158

  IF(  KIND(TZ81)     .NE. 8 )       STOP 161
  IF(  TZ81           .NE. Z81 )     STOP 162
  IF(  KIND(TZ82)     .NE. 8 )       STOP 163
  IF(  ANY(TZ82       .NE. Z82 ))    STOP 164
  IF(  KIND(TZ83)     .NE. 8 )       STOP 165
  IF(  ANY(TZ83       .NE. Z82 ))    STOP 166
  IF(  KIND(TZ84)     .NE. 8 )       STOP 167
  IF(  ANY(TZ84       .NE. Z82 ))    STOP 168

  IF(  KIND(TZ61)     .NE. 16 )      STOP 171
  IF(  TZ61           .NE. Z61 )     STOP 172
  IF(  KIND(TZ62)     .NE. 16 )      STOP 173
  IF(  ANY(TZ62       .NE. Z62 ))    STOP 174
  IF(  KIND(TZ63)     .NE. 16 )      STOP 175
  IF(  ANY(TZ63       .NE. Z62 ))    STOP 176
  IF(  KIND(TZ64)     .NE. 16 )      STOP 177
  IF(  ANY(TZ64       .NE. Z62 ))    STOP 178

  END




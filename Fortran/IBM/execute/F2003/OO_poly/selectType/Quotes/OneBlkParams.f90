! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : ONLY ONE TYPE/CLASS IS BLOCK
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The type parameters are diff
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM OneBlkParams
  IMPLICIT NONE

  INTEGER :: Visited = 0

  IF (                         &
  &      (Fint(1_1) .NE. 1 )   &
  & .OR. (Fint(2_2) .NE. 1 )   &
  & .OR. (Fint(4_4) .NE. 1 )   &
  & .OR. (Fint(8_8) .NE. 1 )   &
  & .OR. (Fint(2_2) .NE. 1 )   ) THEN
    STOP 111
  END IF

  IF (                             &
  &      (FChar("1")    .NE. 1 )   &
  & .OR. (FChar("12")   .NE. 1 )   &
  & .OR. (FChar("")     .NE. 1 )   &
  & .OR. (FChar("123")  .NE. 1 )   &
  & .OR. (FChar("1234") .NE. 1 )   ) THEN
    STOP 112
  END IF

  IF (                                      &
  &      (FCmpx((1.0_4, 0_2))   .NE. 1 )   &
  & .OR. (FCmpx((8_8, 2._4))     .NE. 1 )   &
  & .OR. (FCmpx((1_1, 8._16))    .NE. 1 )   &
  & .OR. (FCmpx((16._16, .8_16)) .NE. 1 )   ) THEN
    STOP 112
  END IF

  CONTAINS

  INTEGER FUNCTION FInt(Arg)
  CLASS(*) :: Arg

  FInt = 0
  SELECT TYPE ( Arg )
    TYPE IS (INTEGER(1))
      Fint = FInt + 1
    TYPE IS (INTEGER(2))
      Fint = FInt + 1
    CLASS DEFAULT
      Fint = FInt + 1
    TYPE IS (INTEGER(4))
      Fint = FInt + 1
    TYPE IS (INTEGER(8))
      Fint = FInt + 1
  END SELECT

  END FUNCTION

  INTEGER FUNCTION FChar(Arg)
  CLASS(*) :: Arg

  FChar = 0
  SELECT TYPE ( Arg )
    TYPE IS (CHARACTER(*))
      FChar = FChar + 1
    CLASS DEFAULT
      FChar = FChar + 1
  END SELECT

  END FUNCTION

  INTEGER FUNCTION FCmpx(Arg)
  CLASS(*) :: Arg

  FCmpx = 0
  SELECT TYPE ( Arg )
    TYPE IS (COMPLEX(4))
      FCmpx = FCmpx + 1
    CLASS DEFAULT
      FCmpx = Fcmpx + 1
    TYPE IS (COMPLEX(8))
      FCmpx = FCmpx + 1
    TYPE IS (COMPLEX(16))
      FCmpx = FCmpx + 1
  END SELECT

  END FUNCTION

  END


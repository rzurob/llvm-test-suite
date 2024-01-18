! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!* Do
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Do2
  IMPLICIT CLASS(DT)(U)
  IMPLICIT CLASS(*)(V)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(3) :: C
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(Int=6, C="123"), i=1,16)/), 6)

  CONTAINS

  SUBROUTINE Sub(U, V)
  DIMENSION :: U(:)
  CLASS(*)  :: V

  SELECT TYPE (U)
  CLASS IS (DT)

    IF (ANY(U%Int   .NE. 6))      ERROR STOP 20
    IF (ANY(U%C     .NE. "123"))   ERROR STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  ERROR STOP 22
      i = 0
      DO While ( i <= U(1)%Int)
        I =i + 1
        CONTINUE
      END DO

  CLASS DEFAULT
    STOP 40
  END SELECT

  SELECT TYPE (V)
  CLASS DEFAULT
    STOP 40
  TYPE IS (INTEGER)
      i = 0
      DO While (I <= V )
        I = I + 1
        CONTINUE
      END DO
  END SELECT

  END SUBROUTINE

  END




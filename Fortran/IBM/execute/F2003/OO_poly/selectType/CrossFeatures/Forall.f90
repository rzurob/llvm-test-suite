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
!* Forall
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Forall
  IMPLICIT CLASS(DT)(U)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(3) :: C
  END TYPE
  INTEGER :: i

  CALL Sub((/(DT(Int=-1, C="123"), i=1,16)/))

  CONTAINS

  SUBROUTINE Sub(U)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT)

    IF (ANY(U%Int   .NE. -1))      ERROR STOP 20
    IF (ANY(U%C     .NE. "123"))   ERROR STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  ERROR STOP 22

    FORALL  (I=1:16 )
      U%Int = 1
      U%C="321"
    END FORALL

    IF (ANY(U%Int .NE. 1))     ERROR STOP 30
    IF (ANY(U%C   .NE. "321")) ERROR STOP 31

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END




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
!*  Target
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      INTEGER(4)      :: IArr(2)=1
      CHARACTER(1025) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM Target1
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V(4,4)

  CALL Sub(V(1:3, 2:4), V%CArr(1))

  ASSOCIATE( V => V(1:3, 2:4) )
  ASSOCIATE( V => V(1:2,2:3) )
    IF (ANY(V%IArr(1) .NE. -1)) STOP 21
    IF (ANY(V%IArr(1) .NE. -1)) STOP 21
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE( V => V%CArr(1) )
  ASSOCIATE( V => V(1:2,2:3) )
    IF (TRIM(V(1,1)) .NE. "1") STOP 62
    IF (TRIM(V(2,2)) .NE. "1") STOP 63
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(U, W)
  CLASS(DT), TARGET  :: U(:,:)
  CLASS(DT), POINTER :: PU(:,:)
  CLASS(*), TARGET  :: W(:,:)
  CHARACTER(1025), POINTER :: PW(:,:)
  INTEGER :: i

  IF (ANY(SHAPE(U) .NE. (/3,3/))) STOP 30
  PU => U(1:2,2:3)
  SELECT TYPE( U => PU  )
  CLASS IS (DT)

    IF (ANY(U%IArr(1) .NE. 1)) STOP 31
    IF (TRIM(U(1,1)%CArr(1)) .NE. "!") STOP 32
    IF (TRIM(U(2,2)%CArr(2)) .NE. "!") STOP 33
    U%IArr(1) = -1
    U%IArr(2) = -2
  CLASS DEFAULT
    STOP 40
  END SELECT

  IF (ANY(SHAPE(W) .NE. (/4,4/))) STOP 40
  SELECT TYPE( W => W  )
  TYPE IS (CHARACTER(*))
    IF (TRIM(W(1,1)) .NE. "!") STOP 42
    IF (TRIM(W(4,4)) .NE. "!") STOP 43

    PW => W(1:2,2:3)
    PW = "1"

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END




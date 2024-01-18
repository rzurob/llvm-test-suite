! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/OO_poly/selectType/CrossFeatures/Target1.f
! opt variations: -qnock -qnodeferredlp

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
    TYPE  :: DT0(K1,K2,N1)    ! (4,1,1025)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: IArr(2)=1
      CHARACTER(kind=K2,len=N1) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1    ! (4,1,1025)
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,1,1025)
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
  TYPE(DT(4,1,1025)), TARGET :: V(4,4)

  CALL Sub(V(1:3, 2:4), V%CArr(1))

  ASSOCIATE( V => V(1:3, 2:4) )
  ASSOCIATE( V => V(1:2,2:3) )
    IF (ANY(V%IArr(1) .NE. -1)) ERROR STOP 21
    IF (ANY(V%IArr(1) .NE. -1)) ERROR STOP 21
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE( V => V%CArr(1) )
  ASSOCIATE( V => V(1:2,2:3) )
    IF (TRIM(V(1,1)) .NE. "1") ERROR STOP 62
    IF (TRIM(V(2,2)) .NE. "1") ERROR STOP 63
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  SUBROUTINE Sub(U, W)
  CLASS(DT(4,1,*)), TARGET  :: U(:,:)
  CLASS(DT(4,1,:)), POINTER :: PU(:,:)
  CLASS(*), TARGET  :: W(:,:)
  CHARACTER(1025), POINTER :: PW(:,:)
  INTEGER :: i

  IF (ANY(SHAPE(U) .NE. (/3,3/))) ERROR STOP 30
  PU => U(1:2,2:3)
  SELECT TYPE( U => PU  )
  CLASS IS (DT(4,1,*))

    IF (ANY(U%IArr(1) .NE. 1)) ERROR STOP 31
    IF (TRIM(U(1,1)%CArr(1)) .NE. "!") ERROR STOP 32
    IF (TRIM(U(2,2)%CArr(2)) .NE. "!") ERROR STOP 33
    U%IArr(1) = -1
    U%IArr(2) = -2
  CLASS DEFAULT
    STOP 40
  END SELECT

  IF (ANY(SHAPE(W) .NE. (/4,4/))) ERROR STOP 40
  SELECT TYPE( W => W  )
  TYPE IS (CHARACTER(*))
    IF (TRIM(W(1,1)) .NE. "!") ERROR STOP 42
    IF (TRIM(W(4,4)) .NE. "!") ERROR STOP 43

    PW => W(1:2,2:3)
    PW = "1"

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END




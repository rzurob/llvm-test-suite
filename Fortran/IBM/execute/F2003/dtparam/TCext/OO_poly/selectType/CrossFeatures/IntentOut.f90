! GB DTP extension using:
! ftcx_dtp -qck -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/IntentOut.f
! opt variations: -qnock -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 28, 2005
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
!*  Intent(out)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,K2,N1)    ! (4,1,1025)
      INTEGER, KIND             :: K1,K2
      INTEGER, LEN              :: N1
      INTEGER(K1)               :: IArr(2)=-1
      CHARACTER(kind=K2,len=N1) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1    ! (4,1,1025)
      CLASS(DT0(K1,K2,:)), POINTER, PRIVATE :: Ptr
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

  PROGRAM IntentOut
  USE M
  IMPLICIT NONE
  TYPE(DT(4,1,1025)), TARGET :: V(2,2)

  CALL Sub(V)

    IF (ANY(V%IArr(1) .NE. 1)) ERROR STOP 30
    IF (ANY(V%IArr(2) .NE. 2)) ERROR STOP 31
    IF (TRIM(V(1,1)%CArr(1)) .NE. "1") ERROR STOP 32
    IF (TRIM(V(2,2)%CArr(2)) .NE. "2") ERROR STOP 33

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT(4,1,*)), INTENT(OUT) :: U(:,:)
  INTEGER :: i

  SELECT TYPE( U )
  CLASS IS (DT(4,1,*))

    IF (ANY(U%IArr(1) .NE. -1)) ERROR STOP 21
    IF (TRIM(U(1,1)%CArr(1)) .NE. "!") ERROR STOP 22
    IF (TRIM(U(2,2)%CArr(2)) .NE. "!") ERROR STOP 23

    U%IArr(1) = 1
    U%IArr(2) = 2
    U%CArr(1) = "1"
    U%CArr(2) = "2"

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END




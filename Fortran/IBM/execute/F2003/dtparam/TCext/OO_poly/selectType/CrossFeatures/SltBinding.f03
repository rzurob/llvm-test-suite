! GB DTP extension using:
! ftcx_dtp -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/SltBinding.f
! opt variations: -qck -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  The selector is a binding func call
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,1025)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)=-1
      CHARACTER(N1) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1    ! (4,1025)
      CLASS(DT0(K1,:)), POINTER, PRIVATE :: Ptr
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,1025)
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM SltBinding
  USE M
  IMPLICIT NONE
  TYPE(DT(4,1025)), TARGET :: V

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT(4,*)), TARGET :: U

  SELECT TYPE(U => U%GetObj(U))
  CLASS IS (DT(4,*))
    SELECT TYPE (U => U%GetObj(U%DT1%DT0))
    CLASS IS (DT(4,*))
      STOP 20
    CLASS IS (DT0(4,*))
      IF (ANY(U%IArr .NE. -1)) ERROR STOP 21
      IF (TRIM(U%CArr(1)) .NE. "!") ERROR STOP 22
      IF (TRIM(U%CArr(2)) .NE. "!") ERROR STOP 23
    CLASS DEFAULT
      STOP 30
    END SELECT
  CLASS DEFAULT
    STOP 31
  END SELECT

  END SUBROUTINE

  END
! GB DTP extension using:
! ftcx_dtp -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/SaveVar.f
! opt variations: -qck -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  Save
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,1025)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)=5
      CHARACTER(N1) :: CArr(2)="5"
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

  PROGRAM SaveVar
  USE M
  IMPLICIT NONE
  TYPE(DT(4,1025)), TARGET :: V(2,2)

  CALL Sub(5)

  CONTAINS

  RECURSIVE SUBROUTINE Sub(I)
  CLASS(DT(4,:)), SAVE, POINTER :: U(:,:)
  INTEGER :: i

  IF(i .EQ. 5) THEN
    ALLOCATE(DT(4,1025) :: U(2,2))
  END IF
  SELECT TYPE( U )
  CLASS IS (DT(4,*))

    IF (ANY(U%IArr(1) .NE. i)) ERROR STOP 21
    IF (TRIM(U(1,1)%CArr(1)) .NE. CHAR(ICHAR("0")+i)) ERROR STOP 22
    IF (TRIM(U(2,2)%CArr(2)) .NE. CHAR(ICHAR("0")+i)) ERROR STOP 23

    U%IArr(1) = i-1
    U%IArr(2) = i-1
    U%CArr(1) = CHAR(ICHAR("0")+i-1)
    U%CArr(2) = CHAR(ICHAR("0")+i-1)

    IF (I .GT. 1) THEN
      CALL Sub(i-1)
    END IF
  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END




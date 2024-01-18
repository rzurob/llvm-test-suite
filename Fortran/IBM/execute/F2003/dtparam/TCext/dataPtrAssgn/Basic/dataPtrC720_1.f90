! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC720_1.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC720_1.f
!*
!*  DATE                       : Feb. 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C720 (R735) If bounds-remapping-list is specified, data-target shall have rank one;
!*  otherwise, the ranks of data-pointer-object and data-target shall be the same.
!*
!*  (322997)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  CONTAINS
    PROCEDURE :: GetID
  END TYPE

  CONTAINS
    ELEMENTAL FUNCTION GetID(Arg)
    CLASS(DT(*,4)), INTENT(IN) :: Arg
    INTEGER   :: GetID
      GetID = Arg%ID
    END FUNCTION
  END MODULE

  PROGRAM dataPtrC720_1
  USE M
  IMPLICIT NONE

  CLASS(DT(:,4)), ALLOCATABLE, TARGET  :: Arr1(:), Arr2(:,:), Arr3(:,:,:)
  CLASS(*), POINTER              :: Ptr1(:), Ptr2(:,:)

  ALLOCATE(Arr2(2,3), SOURCE=DT(20,4)(-1))
  Ptr1(0:1) => Arr2(:,3)

  IF (ANY(LBOUND(Ptr1) .NE.   (/0/))) STOP 11
  IF (ANY(UBOUND(Ptr1) .NE.   (/1/))) STOP 12
  SELECT TYPE ( Ptr1)
  TYPE IS (DT(*,4))
    IF (ANY(Ptr1%GetId() .NE. -1))    STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT


  ALLOCATE(Arr1(8), SOURCE=(/DT(20,4)(1),DT(20,4)(2),DT(20,4)(3),DT(20,4)(4),DT(20,4)(5),DT(20,4)(6),DT(20,4)(7),DT(20,4)(8)/))
  Ptr2(0:1, 0:1) => Arr1(::2)

  IF (ANY(LBOUND(Ptr2) .NE.   (/0,0/))) STOP 21
  IF (ANY(UBOUND(Ptr2) .NE.   (/1,1/))) STOP 22
  SELECT TYPE ( Ptr2)
  TYPE IS (DT(*,4))
    IF (ANY(Ptr2%GetId() .NE. RESHAPE((/1,3,5,7/), (/2,2/)))) STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT

  Arr2(:,3)%ID = 1
  Ptr1(3:) => Arr2(:,3)

  IF (ANY(LBOUND(Ptr1) .NE.   (/3/))) STOP 31
  IF (ANY(UBOUND(Ptr1) .NE.   (/4/))) STOP 32
  SELECT TYPE ( Ptr1)
  TYPE IS (DT(*,4))
    IF (ANY(Ptr1%GetId() .NE. 1))     STOP 33
  CLASS DEFAULT
    STOP 34
  END SELECT

  ALLOCATE(Arr3(2,2,2), SOURCE=DT(20,4)(-1))
  Arr3(:,1,:)%ID = RESHAPE((/1,2,3,4/),(/2,2/))
  Ptr2(0:, 0:) => Arr3(:,1,:)

  IF (ANY(LBOUND(Ptr2) .NE.   (/0,0/))) STOP 41
  IF (ANY(UBOUND(Ptr2) .NE.   (/1,1/))) STOP 42
  SELECT TYPE ( Ptr2)
  TYPE IS (DT(*,4))
    IF (ANY(Ptr2%GetId() .NE. RESHAPE((/1,2,3,4/),(/2,2/)))) STOP 43
  CLASS DEFAULT
    STOP 44
  END SELECT

  DEALLOCATE(Arr1)
  DEALLOCATE(Arr2)
  DEALLOCATE(Arr3)

  END



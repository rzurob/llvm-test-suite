! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/Basic/dataPtrTar1.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrTar1.f
!*
!*  DATE                       : Feb. 04, 2006
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
!*   If data-target is not a pointer, data-pointer-object becomes pointer associated with
!*   the assignment target. Otherwise, the pointer association status of data-pointer-object
!*   becomes that of data-target; if data-target is associated with an object,
!*   data-pointer-object becomes associated with the assignment target.  If data
!*   target is allocatable, it shall be allocated.
!*   -- status
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTar1
  IMPLICIT NONE

  TYPE :: DT0(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE
  TYPE, EXTENDS(DT0) :: DT    ! (4)
  END TYPE

  TYPE(DT(4)), TARGET  :: Arr1(100)
  TYPE(DT(4)), TARGET  :: Arr2(100, 100)

  TYPE(DT(4)),   POINTER  :: Ptr0(:)
  TYPE(DT(4)),   POINTER  :: Ptr1(:)
  TYPE(DT(4)),   POINTER  :: Ptr2(:,:)
  CLASS(DT0(4)), POINTER  :: Ptr3(:)
  CLASS(DT(4)),  POINTER  :: Ptr4(:,:)
  CLASS(*),   POINTER  :: Ptr5(:)

  Ptr0 => Arr1
  Ptr0(1:) => NULL()
  IF ( ASSOCIATED(Ptr0) )       STOP 11

  Ptr0 => Arr1
  Ptr0(1:1) => NULL()
  IF ( ASSOCIATED(Ptr0) )       STOP 12

  ptr0(1:1) => NULL()
  Ptr1(1:)  => Ptr0
  IF ( ASSOCIATED(Ptr1) )       STOP 13

  Ptr0(1:1) => Arr1
  Ptr1(1:1)  => Ptr0
  IF ( .NOT. ASSOCIATED(Ptr1) ) STOP 14

  Ptr2(1:, 1:) => Arr2
  Ptr2(3:, 0:)  => Ptr2
  IF ( .NOT. ASSOCIATED(Ptr2) ) STOP 15

  Ptr4(1: , 1: ) => NULL()
  Ptr2(1: , 2:)  => Ptr4
  IF ( ASSOCIATED(Ptr2) )       STOP 16

! This is not correct
! Ptr4(1:1, 1:1) => NULL()
! Ptr2(1: , 2:   )  => Ptr4
! IF ( ASSOCIATED(Ptr2) )       STOP 16

  Ptr1(1:) => Arr1
  Ptr3(3:)  => Ptr1
  IF ( .NOT. ASSOCIATED(Ptr3) )         STOP 17
  IF ( .NOT. SAME_TYPE_AS(Ptr3, Ptr1) ) STOP 117

  Ptr1(1:) => NULL()
  Ptr3(5:105)  => Ptr1
  IF ( ASSOCIATED(Ptr3) )               STOP 18
  IF ( .NOT. SAME_TYPE_AS(Ptr3, Ptr1) ) STOP 118

  Ptr2 => Arr2
  Ptr4(3:, 100:)  => Ptr2
  IF ( .NOT. ASSOCIATED(Ptr4) )         STOP 19
  IF ( .NOT. SAME_TYPE_AS(Ptr4, Ptr2) ) STOP 119

  Ptr2(5:   , -105: ) => NULL()
  Ptr4(-5:4, 5:5)  => Ptr2(1,:)
  IF ( ASSOCIATED(Ptr4) )               STOP 20
  IF ( .NOT. SAME_TYPE_AS(Ptr4, Ptr2) ) STOP 120

  Ptr1(1:) => NULL()
  Ptr5(3:)  => Ptr1
  IF ( ASSOCIATED(Ptr5) )               STOP 21
  IF ( .NOT. SAME_TYPE_AS(Ptr5, Ptr1) ) STOP 121

  Ptr1(106:) => Arr1
  Ptr5(5:105)  => Ptr1
  IF ( .NOT. ASSOCIATED(Ptr5) )         STOP 22
  IF ( .NOT. SAME_TYPE_AS(Ptr5, Ptr1) ) STOP 122

  END



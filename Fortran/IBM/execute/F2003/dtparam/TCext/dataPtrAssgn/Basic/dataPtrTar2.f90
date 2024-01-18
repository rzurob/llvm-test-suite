! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/Basic/dataPtrTar2.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrTar2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 04, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*   If data-target is not a pointer, data-pointer-object becomes pointer associated with
!*   the assignment target. Otherwise, the pointer association status of data-pointer-object
!*   becomes that of data-target; if data-target is associated with an object, 
!*   data-pointer-object becomes associated with the assignment target.  If data
!*   target is allocatable, it shall be allocated.
!*   -- allocatable 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrTar2 
  IMPLICIT NONE

  TYPE :: DT0(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE
  TYPE, EXTENDS(DT0) :: DT    ! (4,20)
    TYPE(DT(K1,:)),   POINTER  :: Ptr0(:)
    TYPE(DT(K1,:)),   POINTER  :: Ptr1(:)
    TYPE(DT(K1,:)),   POINTER  :: Ptr2(:,:)
    CLASS(DT0(K1,:)), POINTER  :: Ptr3(:)
    CLASS(DT(K1,:)),  POINTER  :: Ptr4(:,:)
    CLASS(*),   POINTER  :: Ptr5(:)
  END TYPE

  TYPE(DT(4,:)), TARGET, ALLOCATABLE  :: Arr1(:)
  TYPE(DT(4,:)), TARGET, ALLOCATABLE  :: Arr2(:, :)

  ALLOCATE(DT(4,20) :: Arr1(100))
  ALLOCATE(DT(4,20) :: Arr2(100, 100))

  Arr1(1)%Ptr0 => Arr1
  Arr1(1)%Ptr0(1:) => NULL()
  IF ( ASSOCIATED(Arr1(1)%Ptr0) )         STOP 11

  Arr1(100)%Ptr0 => Arr1
  Arr1(100)%Ptr0(1:1) => NULL()
  IF ( ASSOCIATED(Arr1(100)%Ptr0) )       STOP 12

  Arr1(100)%ptr0(1:1) => NULL()
  Arr1(100)%Ptr1(1:)  => Arr1(100)%Ptr0
  IF ( ASSOCIATED(Arr1(100)%Ptr1) )       STOP 13

  Arr2(2,2)%Ptr0(1:1) => Arr1
  Arr2(2,2)%Ptr1(1:1)  => Arr2(2,2)%Ptr0
  IF ( .NOT. ASSOCIATED(Arr2(2,2)%Ptr1) ) STOP 14

  Arr1(2)%Ptr2(1:, 1:) => Arr2
  Arr1(2)%Ptr2(3:, 0:)  => Arr1(2)%Ptr2
  IF ( .NOT. ASSOCIATED(Arr1(2)%Ptr2) )   STOP 15

  Arr1(99)%Ptr4(1: , 1: ) => NULL()
  Arr1(99)%Ptr2(1:1, 2:10)  => Arr1(99)%Ptr4(1,::2)
  IF ( ASSOCIATED(Arr1(99)%Ptr2) )         STOP 16

  Arr2(1,1)%Ptr1(1:) => Arr1
  Arr2(1,1)%Ptr3(3:)  => Arr2(1,1)%Ptr1
  IF ( .NOT. ASSOCIATED(Arr2(1,1)%Ptr3) )                   STOP 17
  IF ( .NOT. SAME_TYPE_AS(Arr2(1,1)%Ptr3, Arr2(1,1)%Ptr1) ) STOP 117

  Arr2(1,1)%Ptr1(1:) => NULL()
  Arr2(1,1)%Ptr3(5:105)  => Arr2(1,1)%Ptr1
  IF ( ASSOCIATED(Arr2(1,1)%Ptr3) )                         STOP 18
  IF ( .NOT. SAME_TYPE_AS(Arr2(1,1)%Ptr3, Arr2(1,1)%Ptr1) ) STOP 118

  Arr2(1,1)%Ptr2 => Arr2
  Arr2(1,1)%Ptr4(3:, 100:)  => Arr2(1,1)%Ptr2
  IF ( .NOT. ASSOCIATED(Arr2(1,1)%Ptr4) ) STOP 19

  Arr2(1,1)%Ptr2(5:   , -105: ) => NULL()
  Arr2(1,1)%Ptr4(-5:104, 5:105)  => Arr2(1,1)%Ptr2(::2,105)
  IF ( ASSOCIATED(Arr2(1,1)%Ptr4) ) STOP 20

  Arr2(1,1)%Ptr1(1:) => NULL()
  Arr2(1,1)%Ptr5(3:)  => Arr2(1,1)%Ptr1
  IF ( ASSOCIATED(Arr2(1,1)%Ptr5) )                         STOP 21
  IF ( .NOT. SAME_TYPE_AS(Arr2(1,1)%Ptr5, Arr2(1,1)%Ptr1) ) STOP 121

  Arr2(1,1)%Ptr1(106:) => Arr1
  Arr2(1,1)%Ptr5(5:105)  => Arr2(1,1)%Ptr1
  IF ( .NOT. ASSOCIATED(Arr2(1,1)%Ptr5) )                   STOP 22
  IF ( .NOT. SAME_TYPE_AS(Arr2(1,1)%Ptr5, Arr2(1,1)%Ptr1) ) STOP 122

  END



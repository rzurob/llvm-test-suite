! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Target is a contiguous array section
!*
!*  A nonzero-sized array section is contiguous provided that
!*     (a) its base object is contiguous,
!*     (b) it does not have a vector subscript,
!*     (c) the elements of the section, in array element order, are a subset of the base object elements
!*         that are consecutive in array element order
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM alloctArrTar
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q

      INTEGER, ALLOCATABLE, TARGET   :: A1D(:)
      INTEGER, ALLOCATABLE, TARGET   :: A5D(:,:,:,:,:)

      INTEGER, POINTER              :: ptr(:)
      INTEGER, POINTER              :: ptr5D(:,:,:,:,:)

      INTEGER, POINTER, CONTIGUOUS  :: ptr_c(:)
      INTEGER, POINTER, CONTIGUOUS  :: ptr5D_c(:,:,:,:,:)

      ALLOCATE(A1D(10))
      ALLOCATE(A5D(2,2,2,2,2))

      ptr => A1D
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 10

      ptr => A1D(5:)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 11

      ptr => A1D(1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 12

      ptr => A1D(4:9)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 13

      ptr => A1D(2:10:2)
      IF (       IS_CONTIGUOUS(ptr) )  ERROR STOP 14

      ptr => A1D(5:1:-1)
      IF ( IS_CONTIGUOUS(A1D(5:1:-1)) )  ERROR STOP 15
      IF (         IS_CONTIGUOUS(ptr) )  ERROR STOP 16

      ptr(1:2) => A5D(1,1,1,1,1:2)
      IF ( IS_CONTIGUOUS(A5D(1,1,1,1,1:2)) )  ERROR STOP 17
      IF (              IS_CONTIGUOUS(ptr) )  ERROR STOP 18

      ptr => A1D(2:10:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 19
!*
      L = LBOUND(A1D, 1)
      U = UBOUND(A1D, 1)

      ptr => A1D(L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 20

      ptr => A1D(L+1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 21

      ptr => A1D(:U-1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 22

      ptr => A1D(L+3:U-3:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 23
!*
      ptr_c => ptr
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 24

      ptr_c => ptr(5:)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 25

      ptr_c => ptr(1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 26

      ptr_c => ptr(4:9)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 27

      ptr_c => ptr(2:10:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 28
!*
      ptr => ptr_c
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 29

      ptr => ptr_c(5:)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 30

      ptr => ptr_c(1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 31

      ptr => ptr_c(4:9)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 32

      ptr => ptr_c(2:10:2)
      IF (       IS_CONTIGUOUS(ptr) )  ERROR STOP 33

      ptr => ptr_c(2:10:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 34

      ptr => A5D(1:2,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 35

      ptr_c => A5D(1:2,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) )  ERROR STOP 36

      L = LBOUND(A5D, 1)
      U = UBOUND(A5D, 1)
      ptr_c => A5D(L:U,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) )  ERROR STOP 37

!******************** rank > 1 ******************************

      ptr5D => A5D
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 48

      ptr5D => A5D(:,:,:,:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 49

      ptr5D => A5D(1:,1:,1:,1:,1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 50

      ptr5D => A5D(1:,1:,1:,1:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 51

      ptr5D => A5D(1:2,1:2,1:2,1:2,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 52

      ptr5D => A5D(:,:,:,:,1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 53

      ptr5D => A5D(:,:,:,:,1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 54

      ptr5D => A5D(:,:,:,:,1:2)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 55

      ptr5D => A5D(:,:,:,:,1:2:2)
      IF (       IS_CONTIGUOUS(ptr5D) )  ERROR STOP 56

      ptr5D => A5D(:,:,:,:,2:1:-1)
      IF (       IS_CONTIGUOUS(ptr5D) )  ERROR STOP 57

      ptr5D => A5D(:,:,:,:,1:2:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 58
!*
      L = LBOUND(A5D, 5)
      U = UBOUND(A5D, 5)
      ptr5D => A5D(:,:,:,:,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 59

      ptr5D => A5D(L:,L:,L:,L:,L:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 60

      ptr5D => A5D(L:U,L:U,L:U,:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 61

      ptr5D => A5D(L:U,L:U,L:U,L:U,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 62
!*
      ptr5D_c => ptr5D
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 65

      ptr5D_c => ptr5D(:,:,:,:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 66

      ptr5D_c => ptr5D(1:,1:,1:,1:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 67

      ptr5D_c => ptr5D(1:2,1:2,1:2,1:2,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 68

      ptr5D_c => ptr5D(:2,:2,:2,:2,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 69

      ptr5D_c => ptr5D(:,:,:,:,1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 70

      ptr5D_c => ptr5D(:,:,:,:,1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 71

      ptr5D_c => ptr5D(:,:,:,:,1:2)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) ) ERROR STOP 72
!*
      L = LBOUND(A5D, 5)
      U = UBOUND(A5D, 5)
      P = LBOUND(A5D, 1)
      Q = UBOUND(A5D, 1)
      ptr5D => A5D
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 73

      ptr5D_c => ptr5D(P:,L:,P:,L:,L:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 74

      ptr5D_c => ptr5D(P:Q,L:U,P:U,L:Q,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 75

      ptr5D_c => ptr5D(L:U,L:U,L:U,L:U,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 76

      ptr5D_c => ptr5D(P:Q,P:Q,P:Q,P:Q,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 77

      DEALLOCATE(A5D)
      ALLOCATE(A5D(1,2,3,4,5))

      ptr5D_c => ptr5D(LBOUND(A5D, 1):UBOUND(A5D, 1), &
&                      LBOUND(A5D, 2):UBOUND(A5D, 2), &
&                      LBOUND(A5D, 3):UBOUND(A5D, 3), &
&                      LBOUND(A5D, 4):UBOUND(A5D, 4), &
&                      LBOUND(A5D, 5):UBOUND(A5D, 5))
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 78

END PROGRAM alloctArrTar

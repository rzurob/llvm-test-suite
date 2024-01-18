! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED : The module will be compiled with XLF 13.1
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
PROGRAM arraySectTar1
      USE Mod
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q

      INTEGER, POINTER, CONTIGUOUS  :: ptr_c(:)
      INTEGER, POINTER, CONTIGUOUS  :: ptr5D_c(:,:,:,:,:)

      ptr => I1D
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 10

      ptr => I1D(5:)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 11

      ptr => I1D(1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 12

      ptr => I1D(4:9)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 13

      ptr => I1D(2:10:2)
      IF (       IS_CONTIGUOUS(ptr) )  ERROR STOP 14

      ptr => I1D(5:1:-1)
      IF ( IS_CONTIGUOUS(I1D(5:1:-1)) )  ERROR STOP 15
      IF (         IS_CONTIGUOUS(ptr) )  ERROR STOP 16

      ptr(1:2) => I5D(1,1,1,1,1:2)
      IF ( IS_CONTIGUOUS(I5D(1,1,1,1,1:2)) )  ERROR STOP 17
      IF (              IS_CONTIGUOUS(ptr) )  ERROR STOP 18

      ptr => I1D(2:10:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 19
!*
      L = LBOUND(I1D, 1)
      U = UBOUND(I1D, 1)

      ptr => I1D(L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 20

      ptr => I1D(L+1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 21

      ptr => I1D(:U-1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 22

      ptr => I1D(L+3:U-3:1)
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

      ptr => I5D(1:2,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 35

      ptr_c => I5D(1:2,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) )  ERROR STOP 36

      L = LBOUND(I5D, 1)
      U = UBOUND(I5D, 1)
      ptr_c => I5D(L:U,1,1,1,1)
      IF ( .NOT. IS_CONTIGUOUS(ptr_c) )  ERROR STOP 37

! see defect 385539
!******************* zero sized array ***********************

      !IF ( .NOT. IS_CONTIGUOUS(I0D) )  ERROR STOP 38
      !ptr => I0D
      !IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 39

      !ptr_c => I0D
      !IF ( .NOT. IS_CONTIGUOUS(ptr_c) ) ERROR STOP 40

      !ptr_c => ptr
      !IF ( .NOT. IS_CONTIGUOUS(ptr_c) )  ERROR STOP 41

!************** zero-sized array section ********************

      !ptr => I1D(2:1:1)
      !IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 42
      !IF ( .NOT. IS_CONTIGUOUS(I1D(2:1:1)) )  ERROR STOP 43

      !ptr => I5D(2:1:1,1,1,1,1)
      !IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 44
      !IF ( .NOT. IS_CONTIGUOUS(I5D(2:1:1,1,1,1,1)) )  ERROR STOP 45

      !ptr => I5D(1,1,1,1,1:2:-1)
      !IF ( .NOT. IS_CONTIGUOUS(ptr) )  ERROR STOP 46
      !IF ( .NOT. IS_CONTIGUOUS(I5D(1,1,1,1,1:2:-1)) )  ERROR STOP 47

!******************** rank > 1 ******************************

      ptr5D => I5D
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 48

      ptr5D => I5D(:,:,:,:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 49

      ptr5D => I5D(1:,1:,1:,1:,1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 50

      ptr5D => I5D(1:,1:,1:,1:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 51

      ptr5D => I5D(1:2,1:2,1:2,1:2,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 52

      ptr5D => I5D(:,:,:,:,1:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 53

      ptr5D => I5D(:,:,:,:,1:0)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 54

      ptr5D => I5D(:,:,:,:,1:2)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 55

      ptr5D => I5D(:,:,:,:,1:2:2)
      IF (       IS_CONTIGUOUS(ptr5D) )  ERROR STOP 56

      ptr5D => I5D(:,:,:,:,2:1:-1)
      IF (       IS_CONTIGUOUS(ptr5D) )  ERROR STOP 57

      ptr5D => I5D(:,:,:,:,1:2:1)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 58
!*
      L = LBOUND(I5D, 5)
      U = UBOUND(I5D, 5)
      ptr5D => I5D(:,:,:,:,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 59

      ptr5D => I5D(L:,L:,L:,L:,L:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 60

      ptr5D => I5D(L:U,L:U,L:U,:,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 61

      ptr5D => I5D(L:U,L:U,L:U,L:U,L:U)
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
      L = LBOUND(I5D, 5)
      U = UBOUND(I5D, 5)
      P = LBOUND(I5D, 1)
      Q = UBOUND(I5D, 1)
      ptr5D => I5D
      IF ( .NOT. IS_CONTIGUOUS(ptr5D) )  ERROR STOP 73

      ptr5D_c => ptr5D(P:,L:,P:,L:,L:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 74

      ptr5D_c => ptr5D(P:Q,L:U,P:U,L:Q,:)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 75

      ptr5D_c => ptr5D(L:U,L:U,L:U,L:U,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 76

      ptr5D_c => ptr5D(P:Q,P:Q,P:Q,P:Q,L:U)
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 77

      ptr5D_c => ptr5D(LBOUND(I5D, 1):UBOUND(I5D, 1), &
&                      LBOUND(I5D, 2):UBOUND(I5D, 2), &
&                      LBOUND(I5D, 3):UBOUND(I5D, 3), &
&                      LBOUND(I5D, 4):UBOUND(I5D, 4), &
&                      LBOUND(I5D, 5):UBOUND(I5D, 5))
      IF ( .NOT. IS_CONTIGUOUS(ptr5D_c) )  ERROR STOP 78

END PROGRAM arraySectTar1

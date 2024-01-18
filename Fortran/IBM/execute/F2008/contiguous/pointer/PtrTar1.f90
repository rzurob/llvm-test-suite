! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Pointer has contiguous attribute
!*
!*    Dummy argument is pointer with no CONTIGUOUS attribute
!*    Actual argument is contiguous
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
PROGRAM PtrTar1
      IMPLICIT NONE

      INTEGER, TARGET  :: I3D(2,2,2)
      INTEGER, POINTER :: ptr(:,:,:)

      ptr => I3D
      CALL Sub1(ptr)
      CALL Sub2(ptr)

      ALLOCATE( ptr(2,2,2) )
      CALL Sub1(ptr)
      CALL Sub2(ptr)

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER, POINTER :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

        IF (       ASSOCIATED(ptr)    ) ERROR STOP 10
        IF ( .NOT. ASSOCIATED(Arg)    ) ERROR STOP 11
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 12

        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 13
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 14

        CALL SubSub(ptr)
        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 15
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 16

      END SUBROUTINE Sub1

      SUBROUTINE Sub2(Arg)
        INTEGER, POINTER :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

        IF (       ASSOCIATED(ptr)    ) ERROR STOP 20
        IF ( .NOT. ASSOCIATED(Arg)    ) ERROR STOP 22
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 23

        CALL SubSub(arg)
        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 24
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 25

      END SUBROUTINE Sub2

      SUBROUTINE SubSub(Arg)
        INTEGER, TARGET  :: Arg(:,:,:)
        INTEGER, POINTER :: ptr(:,:,:)

        IF (      ASSOCIATED(ptr)     ) ERROR STOP 26
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 27

        ptr => Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 28
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 29
      END SUBROUTINE SubSub

END PROGRAM PtrTar1

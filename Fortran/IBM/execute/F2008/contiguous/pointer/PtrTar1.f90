! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : PtrTar1.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
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

        IF (       ASSOCIATED(ptr)    ) STOP 10
        IF ( .NOT. ASSOCIATED(Arg)    ) STOP 11
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 12

        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 13
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 14

        CALL SubSub(ptr) 
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 15
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 16

      END SUBROUTINE Sub1
      
      SUBROUTINE Sub2(Arg)
        INTEGER, POINTER :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

        IF (       ASSOCIATED(ptr)    ) STOP 20
        IF ( .NOT. ASSOCIATED(Arg)    ) STOP 22
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 23

        CALL SubSub(arg)
        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 24
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 25

      END SUBROUTINE Sub2

      SUBROUTINE SubSub(Arg)
        INTEGER, TARGET  :: Arg(:,:,:)
        INTEGER, POINTER :: ptr(:,:,:)

        IF (      ASSOCIATED(ptr)     ) STOP 26
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 27

        ptr => Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 28
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 29
      END SUBROUTINE SubSub

END PROGRAM PtrTar1

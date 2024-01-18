! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : isContigTrue1.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED : unlimited polymorphic
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - 
!*                                
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
PROGRAM isContigTrue1
      IMPLICIT NONE

      INTEGER, TARGET :: Iarr(5,5)
      CLASS(*), POINTER :: ptr(:,:)
      CONTIGUOUS :: ptr 

      Iarr = 1
      IF ( .NOT. IS_CONTIGUOUS(Iarr) )       ERROR STOP 10

      ptr => Iarr 
      SELECT TYPE ( s => ptr )
          TYPEIS (INTEGER)
              IF ( .NOT. IS_CONTIGUOUS(s) )  ERROR STOP 60

          CLASS DEFAULT
              ERROR STOP 61
      END SELECT
 
      CALL Sub1(Iarr)

      CALL Sub2(ptr)

      CALL Sub3(Iarr)

      ptr => foo(Iarr) 

      CONTAINS

      SUBROUTINE Sub1(Arg)
        CLASS(*), POINTER :: ptr(:,:)
        CLASS(*), TARGET, CONTIGUOUS :: Arg(:,:)

        IF (      ASSOCIATED(ptr)     ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 41

        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 42

      END SUBROUTINE Sub1

      SUBROUTINE Sub2(Arg)
        CLASS(*), POINTER, CONTIGUOUS :: Arg(:,:)
        CLASS(*), POINTER, CONTIGUOUS :: ptr(:,:)

        IF (       ASSOCIATED(ptr)    ) STOP 40
        IF ( .NOT. ASSOCIATED(Arg)    ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 41

        ptr=>Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 42

      END SUBROUTINE Sub2

      SUBROUTINE Sub3(Arg)
        CLASS(*) :: Arg(:,:)
        CLASS(*), POINTER, CONTIGUOUS :: ptr(:,:)

        IF (      ASSOCIATED(ptr)     ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 41

        ALLOCATE( ptr(UBOUND(Arg,1),UBOUND(Arg,2)), SOURCE = Arg )
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 42

      END SUBROUTINE Sub3

      FUNCTION foo(Arg)
        CLASS(*), CONTIGUOUS, TARGET :: Arg(:,:)
        CLASS(*), POINTER :: foo(:,:)

        IF (      ASSOCIATED(foo)     ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 41

        foo => Arg
        IF ( .NOT. ASSOCIATED(foo)    ) STOP 40
        IF ( .NOT. IS_CONTIGUOUS(foo) ) STOP 42
      END FUNCTION

END PROGRAM isContigTrue1

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Actual argument array pointer
!*                                 with contiguous attribute
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
MODULE Mod
      INTEGER, TARGET  :: T1(1024)
      INTEGER, POINTER, CONTIGUOUS  :: p1(:), p2(:)

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, POINTER :: Arg(:)
        INTEGER, POINTER :: ptr(:)

        IF (       ASSOCIATED(ptr)    ) ERROR STOP 10
        IF ( .NOT. ASSOCIATED(Arg)    ) ERROR STOP 11
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 12
        IF ( ANY(Arg .NE. [(I, I=1,1024)]) ) ERROR STOP 13

        ptr => Arg

        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 14
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 15
        IF ( ANY(Arg .NE. [(I, I=1,1024)]) ) ERROR STOP 16

      END SUBROUTINE Sub
END MODULE
PROGRAM ptrAssgnDummyTar
      USE Mod

      T1 = [(I, I=1,1024)]
      p1 => T1

      CALL Sub(p1)

      ALLOCATE( p2(1024), SOURCE = [(I, I=1,1024)] )

      CALL Sub(p2)
END PROGRAM ptrAssgnDummyTar

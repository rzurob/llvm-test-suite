! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  NO Intent - INTENT(IN)/INTENT(OUT)/INTENT(INOUT)
!*
!*  (304791)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Intent2
  IMPLICIT TYPE(Base)(P)

  TYPE :: Base
    CHARACTER(3) :: C
  END TYPE

  CONTAINS

    SUBROUTINE IntSub1(Proc)
    PROCEDURE(), INTENT(IN) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub2(Proc)
    PROCEDURE(), INTENT(OUT) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub3(Proc)
    PROCEDURE(), INTENT(INOUT) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub4(Proc)
    PROCEDURE(TYPE(Base)), INTENT(IN) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub5(Proc)
    PROCEDURE(TYPE(Base)), INTENT(OUT) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub6(Proc)
    PROCEDURE(TYPE(Base)), INTENT(INOUT) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub7(Proc)
    INTENT(IN) :: Proc
    EXTERNAL   :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub8(ProcPtr)
    INTENT(IN)             :: ProcPtr   ! This is correct
    PROCEDURE(), POINTER   :: ProcPtr
    END SUBROUTINE

  END


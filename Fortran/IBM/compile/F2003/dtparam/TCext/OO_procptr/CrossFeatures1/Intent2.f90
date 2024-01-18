! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Intent2.f
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Intent2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Intent2.f
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
  IMPLICIT TYPE(Base(1,3))(P)

  TYPE :: Base(K1,N1)    ! (1,3)
    INTEGER, KIND             :: K1
    INTEGER, LEN              :: N1
    CHARACTER(kind=K1,len=N1) :: C
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
    PROCEDURE(TYPE(Base(1,3))), INTENT(IN) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub5(Proc)
    PROCEDURE(TYPE(Base(1,3))), INTENT(OUT) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub6(Proc)
    PROCEDURE(TYPE(Base(1,3))), INTENT(INOUT) :: Proc
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



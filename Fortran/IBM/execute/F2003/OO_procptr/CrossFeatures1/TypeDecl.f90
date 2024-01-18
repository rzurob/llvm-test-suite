! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: TypeDecl.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl.f
!*
!*  DATE                       : Jun. 07, 2005
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
!*  Should not have EXTERNAL or INTRINSIC attribute
!*  specified unless it is a function
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Save
  IMPLICIT TYPE(Base)(P)

  TYPE :: Base
    CHARACTER(3) :: C
  END TYPE

  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr => NULL()

  CALL IntSub1(ProcPtr)
  CALL IntSub2(ProcPtr)
  CALL IntSub3(ProcPtr)
  CALL IntSub4(ProcPtr)
  CALL IntSub5(ProcPtr)

  CONTAINS

    SUBROUTINE IntSub1(Proc)
    TYPE(Base)  :: Proc
    PROCEDURE(TYPE(Base)) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub2(Proc)
    TYPE(Base)  :: Proc
    PROCEDURE() :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub3(Proc)
    TYPE(Base)                     :: Proc
    PROCEDURE(TYPE(Base)), POINTER :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub4(ProcPtr)
    TYPE(Base)                     :: ProcPtr
    PROCEDURE(TYPE(Base)), POINTER :: ProcPtr
    END SUBROUTINE

    SUBROUTINE IntSub5(ProcPtr)
    IMPLICIT NONE
    PROCEDURE(), POINTER :: ProcPtr
    TYPE(Base)           :: ProcPtr
    END SUBROUTINE


  END



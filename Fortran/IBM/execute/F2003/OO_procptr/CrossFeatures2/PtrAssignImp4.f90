! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignImp4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignImp4.f
!*
!*  DATE                       : Mar. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  If proc-target and proc-pointer-object are functions,
!*  they shall have the same type; corresponding type parameters
!*  shall either both be deferred or both have the same value.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION ExtFun1(Arg)
  CLASS(*)          :: Arg(:)
  CLASS(*), POINTER :: ExtFun1(:)
    ALLOCATE(ExtFun1(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  FUNCTION ExtFun2(Arg)
  CLASS(*) :: Arg(:)
  CHARACTER(*)  :: ExtFun2
    SELECT TYPE (Arg)
    TYPE IS (CHARACTER(*))
      ExtFun2 = Arg(1)(1:3)
    CLASS DEFAULT
      STOP 111
    END SELECT
  END FUNCTION

  PROGRAM PtrAssignImp4
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun1(Arg)
     CLASS(*)          :: Arg(:)
     CLASS(*), POINTER :: IFun1(:)
    END FUNCTION

    FUNCTION IFun2(Arg)
      CLASS(*) :: Arg(:)
      CHARACTER(3) :: IFun2
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun1)           :: ExtFun1
  PROCEDURE(IFun1),  POINTER :: ProcPtr1

  PROCEDURE(IFun2)           :: ExtFun2
  PROCEDURE(IFun2),  POINTER :: ProcPtr2

  INTEGER                    :: I

  ProcPtr1 => ExtFun1
  SELECT TYPE ( As1=>ProcPtr1((/((i,-i), i=1, 4098) /)) )
  TYPE IS (COMPLEX)
    SELECT TYPE ( As2 => ExtFun1((/((i,-i), i=1, 4098) /) ) )
    TYPE IS (COMPLEX)
      IF ( ANY( As1 .NE. As2) ) STOP 11
    CLASS DEFAULT
      STOP 12
    END SELECT
  CLASS DEFAULT
    STOP 13
  END SELECT

  ProcPtr2 => ExtFun2
  IF ( ProcPtr2((/("IBM", i=1, 4098)/)) .NE. 'IBM' ) STOP 21

  END


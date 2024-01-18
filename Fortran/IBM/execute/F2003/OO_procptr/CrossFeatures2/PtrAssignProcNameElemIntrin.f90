! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignProcNameElemIntrin.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameElemIntrin.f
!*
!*  DATE                       : Mar. 18, 2005
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
!*  The target is an elemental intrinsics
!*  (315208/318525)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM PtrAssignProcNameElemIntrin
  IMPLICIT NONE

  INTERFACE
    FUNCTION IS1(Arg)
      INTEGER :: IS1
      INTEGER, INTENT(IN) :: Arg
    END FUNCTION
    FUNCTION IS2(Arg)
      REAL :: IS2
      REAL, INTENT(IN) :: Arg
    END FUNCTION
    FUNCTION IS3(Arg)
      REAL :: IS3
      COMPLEX, INTENT(IN) ::  Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IS1), POINTER :: Ptr1
  PROCEDURE(IS2), POINTER :: Ptr2
  PROCEDURE(IS3), POINTER :: Ptr3

  TYPE :: DT
    PROCEDURE(IS2), NOPASS, POINTER :: Ptr
  END TYPE

  TYPE(DT) :: V
  INTEGER  :: B1(100,100)
  REAL     :: B2(100,100)
  INTRINSIC   ABS!, IABS, CABS

  Ptr1 => IABS
  B1 = Ptr1 (-3)
  IF ( ANY(B1 .NE. 3) ) STOP 11

  Ptr2 => ABS
  B2 = Ptr2 (-3.)
  IF ( ANY(B2 .NE. 3.) ) STOP 12

  Ptr3 => CABS
  B2 = Ptr3 ((-3.,-4.))
  IF ( ANY(B2 .NE. 5.) ) STOP 13

  V%Ptr => ABS

  CALL IntSub(Ptr1, Ptr2, Ptr3)
  CALL IntSub(Ptr1, V%Ptr, Ptr3)

  CONTAINS

  SUBROUTINE IntSub(Fun, Ptr1, Ptr2)
  PROCEDURE(IS1)          :: Fun
  PROCEDURE(IS2), POINTER :: Ptr1
  PROCEDURE(IS3), POINTER :: Ptr2
  INTEGER :: A1(100,100)
  REAL    :: A2(100,100)

    A1 = Fun (-3)
    IF ( ANY(A1 .NE. 3) ) STOP 21

    A2 = Ptr1 (-3.)
    IF ( ANY(A2 .NE. 3.) ) STOP 31

    A2 = Ptr2 ((-3., -4.))
    IF ( ANY(A2 .NE. 5.) ) STOP 31

  END SUBROUTINE

  END


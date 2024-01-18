!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram03f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : 6 December, 2012
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : submodule
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   Access public, private, protected variables via host association
!*   in a submodule
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE m
IMPLICIT NONE
integer, public :: a = 2
integer, allocatable :: aaa
integer, protected :: bbb = 2
private aaa
  INTERFACE 
    module function func(x)
      integer, intent(in) :: x
      integer :: func
    end function func

    module subroutine sub
    end subroutine sub

  END INTERFACE
END MODULE m

SUBMODULE (m) b
CONTAINS
  module procedure sub
    allocate(aaa)
    aaa = bbb
  end
END SUBMODULE b

SUBMODULE (m:b) c
CONTAINS
  module procedure func
    integer, intent(in) :: x
    integer :: func
    call sub()
    func = x * aaa
  end procedure
END SUBMODULE c

PROGRAM module_subprogram03f
USE m

if (func(a).ne.4) error stop 66

END PROGRAM module_subprogram03f

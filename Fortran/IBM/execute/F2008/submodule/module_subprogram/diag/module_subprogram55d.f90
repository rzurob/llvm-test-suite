!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : module_subprogram54d
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
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Adding a prefix such as elemental, pure, impure, recursive, etc, 
!*   in the MODULE PROCEDURE statement.
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!234567890923456789092345678909234567890923456789092345678909234567890
MODULE m
integer a
implicit none
  INTERFACE
    impure module subroutine impsub(x)
      integer x
    end subroutine
  
    module impure function impfunc(x)
      integer x
      real :: func2
    end function

    elemental module subroutine elemsub(x)
      integer, intent(in) :: x
    end subroutine
  
    module elemental function elemfunc(x)
      integer, intent(in) :: x
      real :: func2
    end function

  END INTERFACE
END MODULE m

SUBMODULE (m) n
CONTAINS
  ! for a function
  module impure procedure impfunc
    func2 = 3 * x
  end procedure impfunc

  ! for a subroutine
  impure module procedure impsub
    a = 2 * x
  end procedure impsub
 
  ! for a function
  elemental module procedure elemfunc
    func2 = 3 * x
  end procedure elemfunc

  ! for a subroutine
  module elemental procedure elemsub
    a = x
  end procedure elemsub
END SUBMODULE n

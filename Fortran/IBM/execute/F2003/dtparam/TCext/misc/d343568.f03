!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov 07,2007
!*
!*  PRIMARY FUNCTIONS TESTED   : function prefix
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Defect Number 343568
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type :: base(n1)
    integer, len      :: n1
    integer           :: l_1 = 20
    character(len=n1) :: name_1 = 'default_nested_1'
  end type

  type :: child(n2)
    integer, len      :: n2
    integer           :: l_2 = 20
    character(len=n2) :: name_2 = 'default_nested_2'
    type(base(n2))    :: d
  end type

  contains

  character(20) function getName_1 (c)
    class(child(*)),intent(in) :: c
    getName_1 = c%name_2
  end function

  character(20) function getName_2 (c)            ! returns truncated string
    class(child(*)),intent(in) :: c
    getName_2 = c%d%name_1
  end function
end module

use m
type(child(20)) :: e
print *,'getName_1=',getName_1(e)
print *,'getName_2=',getName_2(e)
end

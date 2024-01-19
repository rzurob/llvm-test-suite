! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/19/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               defect 335754 from David
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type base (n)
     integer, len :: n
  end type
contains

  function genBase4 (i)
    integer, intent(in) :: i

    type(base(i)) genBase4, b

    genBase4 = b
  end function
end module

use m
end

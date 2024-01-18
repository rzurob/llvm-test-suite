!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/08/2008
!*  DESCRIPTION                : miscellaneous (defect 342606)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base(k1)
      integer, kind :: k1                               ! OK if not param
      integer(k1) :: baseid = 1
      procedure(getbaseid), pass, pointer :: procptr1   ! OK if no procptr
    end type

    contains

    function getbaseid(arg)
    class(base(4)), intent(in) :: arg
    integer :: getbaseid
      getbaseid = 1
    end function
end module

use m
end

!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d345974
!*
!*  DATE                       : 2008-01-17
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM:ICE:ASTI:AC-IMPDO:DUMMYARG:CLASS(*):
!*                               W-Code Problem?
!*  DESCRIPTION                :
!*
!*  NOTE:  Chris Lord has suggested that this failure might be a W-Code
!*  problem.
!*
!*  The Reduced Code below ICEs in ASTI.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  type, public :: base(l2,k2)    ! (20,4)
     integer, kind :: k2
     integer, len  :: l2
     integer(4)   :: val = -1
  end type base

contains

  subroutine test(arr,t)
    class (*) :: arr(:)
    integer :: t
    i = 1
  end subroutine test

end module mod

program d345974
  use mod

  call test((/ base(20,4):: (base(20,4)(),i=5,4) /), 0)

end program d345974

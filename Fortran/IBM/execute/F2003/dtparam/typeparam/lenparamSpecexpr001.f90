!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: length parameter for character
!                               component and array components' bounds.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l, lb, ub)
        integer, len :: l, lb, ub

        character(l) :: desc(lb:ub) = 'default'
    end type
end module

program lenparamSpecexpr001
use m
    type (base(11, 0, ub=2)) b1
    type (base(30, -1, 10)), allocatable :: b2(:)
!    type (base(20, ub=-1, lb=10)) b3(10000)

    allocate(b2(5))

    b1%desc = (/'b1 0: main program', 'b1 1: main program', &
                'b1 2: main program'/)

    b2(1)%desc = (/'b2(1): -1', 'b2(1): 00','b2(1): 01', 'b2(1): 02', 'b2(1): 03', &
                    'b2(1): 04', 'b2(1): 05', 'b2(1): 06', 'b2(1): 07', &
                    'b2(1): 08', 'b2(1): 09', 'b2(1): 10'/)

    b2(2)%desc = (/'b2(2): -1', 'b2(2): 00','b2(2): 01', 'b2(2): 02', 'b2(2): 03', &
                    'b2(2): 04', 'b2(2): 05', 'b2(2): 06', 'b2(2): 07', &
                    'b2(2): 08', 'b2(2): 09', 'b2(2): 10'/)


    print *, b1
    print *, b2(3:1:-1)

!    if (size(b3(2)%desc) /= 0) error stop 1_4
end

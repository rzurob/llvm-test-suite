!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: use of length type parameters in the
!                               specification expressions for the components;
!                               max and min.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l, lb, ub)
        integer, len :: l, lb=1, ub

        character(l) :: desc(min(lb, ub):max(lb, ub)) = 'default'
    end type
end module

program lenparamSpecexpr003
use m
    type (base(20, ub=0)) b1
    type (base(30, lb=20, ub=10)), allocatable :: b2(:)

    allocate (b2(10))

    b1%desc = (/'xlftest F2003 feature', 'F2003 feature xlftest'/)

    b2(2)%desc(10:11) = 'again: '//b1%desc
    b2(3)%desc(19:20) = b2(2)%desc(10:11)

    if ((lbound(b1%desc, 1) /= 0) .or. (ubound(b1%desc, 1) /= 1) .or. &
        (len (b1%desc) /= 20)) error stop 1_4

    if ((lbound(b2(10)%desc, 1) /= 10) .or. (ubound(b2(9)%desc, 1) /= 20) .or.&
        (len (b2(2)%desc) /= 30)) error stop 2_4

    if (any (b1%desc /= (/'xlftest F2003 featur', 'F2003 feature xlftes'/))) &
            error stop 3_4

    if (any(b2(3)%desc(19:20) /= (/'again: xlftest F2003 featur', &
        'again: F2003 feature xlftes'/))) error stop 5_4
end

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/23/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Data target in the structure constructor;
!                               use intrinsic type target.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k), pointer :: data
        integer(k) :: id(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
    end type
end module

program dtparamConstr005
use m
    class(base(8,:)), allocatable :: b1
    class(base(4,:)), allocatable :: b2

    double precision, target :: d1
    real(4), target :: r1

    allocate (b1, source=base(8, 10)(d1, (/(i, i=1,10)/)))

    allocate (b2, source=child(4, 20, 20)(r1, (/(i*2_8**24, i=1, 20)/), &
            'b2 in main program'))

    if ((.not. associated(b2%data, r1)) .or. &
        (.not. associated(b1%data, d1))) error stop 1_4

    if (any(b2%id(1:10) / 2_8**20 /= 16*b1%id)) error stop 2_4

    if (any(b2%id(11:20)/2_8**21 /= 8*(/(i, i=11, 20)/))) error stop 3_4

    select type (b2)
        type is (child(4, *,*))
            if (b2%name /= 'b2 in main program') error stop 4_4

        class default
            error stop 5_4
    end select
end

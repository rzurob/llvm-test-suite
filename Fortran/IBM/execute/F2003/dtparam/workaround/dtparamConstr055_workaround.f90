!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Test that the allocatable components are
!                               allocated with the same type parameters as that
!                               of the expression; use pointer arrays as the
!                               data-source; also test that the bounds are set
!                               correctly.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type string (n)
        integer, len :: n

        character(n), allocatable :: rep
    end type

    type base (k)
        integer, kind :: k

        integer(k) :: id
        type(string(:)), allocatable :: name(:)
    end type
end module

program dtparamConstr055
use m
    type(string(:)), allocatable :: names(:)

    type (base(4)), pointer :: b1

    allocate (string(10) :: names(-10:10))

    names = (/(string(10)('xlftest '//achar(10+i)), i=-10,10)/)

!    allocate (b1, source=base(4)(100, names))
    allocate(b1)
    b1%id = 100
    call move_alloc(names, b1%name)

    !! verify b1
    if (b1%id /= 100) error stop 1_4

    if (.not. allocated(b1%name)) error stop 2_4

    if ((lbound(b1%name,1) /= -10) .or. (ubound(b1%name,1) /= 10)) &
            error stop 3_4

    if (b1%name%n /= 10) error stop 4_4

    do i = 0, 20
        if (.not. allocated(b1%name(i-10)%rep)) error stop 5_4

        if (b1%name(i-10)%rep /= 'xlftest '//achar(i)) error stop 6_4
    end do

    deallocate(b1)
end

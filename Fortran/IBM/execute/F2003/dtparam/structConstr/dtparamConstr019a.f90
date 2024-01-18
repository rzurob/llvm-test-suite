!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: The derived type with default type
!                               parameter values; for the case where generic
!                               name and type name are the same.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l, n)
        integer, kind :: k = 8
        integer, len :: l = 20, n = 15

        character(l) :: name
        real(k) :: data(n)
    end type

    interface base
        module procedure genBase8
    end interface

    contains

    function genBase8 (len, isize, name, d1)
        integer, intent(in) :: len, isize
        character(*), intent(in) :: name
        real(8), intent(in) :: d1(isize)

        type(base(8, len, isize)) genBase8

        genBase8%name = name
        genBase8%data = d1
    end function
end module

program dtparamConstr019a
use m
    class(base(8,:,:)), allocatable :: b1(:)

    type (base) :: b2

    logical(4), external :: precision_r8

    character(30) :: c1 = 'variable in main program'

    double precision d1(100)

    call random_number (d1)


!    allocate (b1(10), source=(/(base(10, 300, 'xlftest', &
!            (/(i*1.0d3+j, j=1, 300)/)), i=1, 10)/))

    allocate (base(8,10, 300) :: b1(10))

    select type (b1)
        type is(base(8,*,*))
            do i = 1, 10
                b1(i) = base(10, 300, 'xlftest', (/(i*1.0d3+j, j=1, 300)/))
            end do

        class default
            stop 100
    end select

    b2 = base(c1, d1(3:62:4))

    !! verify
    do i = 1, 10
        if (b1(i)%name /= 'xlftest') error stop 1_4

        do j = 1, 300
            if (.not. precision_r8 (b1(i)%data(j), i*1.0d3+j)) error stop 2_4
        end do
    end do


    if (b2%name /= 'variable in main pro') error stop 3_4

    do i = 1, 15
        if (.not. precision_r8 (b2%data(i), d1(4*i-1))) error stop 4_4
    end do
end

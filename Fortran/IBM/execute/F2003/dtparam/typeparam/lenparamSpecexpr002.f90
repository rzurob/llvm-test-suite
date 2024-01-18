!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: length type parameters for
!                               components' length type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n = 10

        real(k) :: data (n)
    end type

    type container (l)
        integer, len :: l = 20

        character(l) :: name
        type(base(8, l)) :: b1
        class(base(4, l+10)), pointer :: b2 => null()
    end type
end module

program lenparamSpecexpr002
use m
    type (container(10)) co1(2)
    class (container), allocatable :: co2

    logical(4), external :: precision_r4, precision_r8

    allocate(co2)
    allocate (co1(1)%b2)

    co1(1)%name = 'co1(1)'
    co1(1)%b1%data = (/(i*1.0d0, i=1, 10)/)
    co1(1)%b2%data = (/(i*1.0e1, i=1, 20)/)

    co1(2) = co1(1)

    co1(2)%name(index(co1(2)%name, '(1)'):) = '(2) 123456'
    co1(2)%b1%data = co1(2)%b1%data * 3.1d55

    co2%name = 'xlftest F2003 feature: derived type'
    co2%b1%data = (/(i*1.5d40+2.d42, i=1, 20)/)


    !! verify the results
    if (co1(2)%name /= 'co1(2) 123') error stop 1_4

    do i = 1, 10
        if (.not. precision_r8(co1(2)%b1%data(i), i*3.1d55)) error stop 2_4
    end do

    do i = 1, 20
        if (.not. precision_r4(co1(2)%b2%data(i), i*1.0e1)) error stop 3_4
    end do

    if (co2%name /= 'xlftest F2003 featur') error stop 4_4

    do i = 1, 20
        if (.not. precision_r4(co2%b1%data(i), i*1.5d40+2.d42)) error stop 5_4
    end do

    if (associated(co2%b2)) error stop 6_4
end

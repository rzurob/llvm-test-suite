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
!*  DATE                       : 01/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: used in function
!                               prefix-spec.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (ki, kr)
        integer, kind :: ki, kr

        integer(ki), pointer :: id => null()
        real(kr), allocatable :: data(:)
    end type

    integer, parameter :: i_default = kind(1)
    integer, parameter :: r_default = kind(1.0)

    contains

    type(base(i_default, kr=r_default)) function genBaseDefault (id, data)
        integer, intent(in) :: id
        real, intent(in) :: data(:)

        allocate (genBaseDefault%id, source=id)
        allocate (genBaseDefault%data(size(data)), source=data)
    end function
end module

program kindparamFuncPrefix001
use m
    interface
        class(base(8, 8)) function genBase8 (id, data)
        use m
            allocatable :: genBase8
            integer(8), intent(in) :: id
            real(8), intent(in), optional :: data(:)
        end function
    end interface

    real r1(100)
    real(8) d1(200)

    type (base(kr=4, ki=4)) b1
    type (base(ki=8, kr=8)), allocatable :: b2(:)

    logical(4), external :: precision_r4, precision_r8

    r1 = (/(i*1.0, i=1, 100)/)
    d1 = 1.2d0*(/r1, r1(100:1:-1)/)

    allocate (b2(100))

    b1 = genBaseDefault (100, r1)

    b2(1) = genBase8 (1_8, d1(20:))
    b2(10) = genBase8(2_8**33)

    b2(30) = b2(1)
    b2(49) = b2(10)

    !! verify results
    if (b1%id /= 100) error stop 1_4

    do i = 1, 100
        if (.not. precision_r4(b1%data(i), r1(i))) error stop 2_4
    end do

    if ((b2(30)%id /= 1) .or. (b2(49)%id/2**23 /= 1024))  error stop 2_4

    if (allocated(b2(10)%data) .or. allocated(b2(49)%data)) error stop 3_4

    do i = 1, 181
        if (.not. precision_r8(b2(30)%data(i), d1(19+i))) error stop 4_4
    end do
end


class(base(8, 8)) function genBase8 (id, data)
use m
    allocatable :: genBase8
    integer(8), intent(in) :: id
    real(8), intent(in), optional :: data(:)

    allocate (genBase8)
    allocate (genBase8%id, source=id)

    if (present(data)) then
        allocate (genBase8%data(size(data)), source=data)
    end if
end function

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
!*  DATE                       : 11/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement An extended type includes all of the
!                               type parameters of its parent.
!                               Case: extended type in a subroutine.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, l)
        integer, kind :: k = 4
        integer, len :: l = 10

        real(k), pointer :: data(:) => null()
        integer(k) :: ids(l)
    end type

    logical(4) precision_r8, precision_x8
    external precision_r8, precision_x8
end module

program dtparamExtends012
    external test1
    call test1
end

subroutine test1
use m
    type, extends(base) :: child
        complex(k), allocatable :: cx
    end type

    real(4), target :: r1(10)
    integer(8) j, int4bound
    parameter (int4bound = 2_8**32_8)

    type (child) c1

    type (child(8, 5)) c2
    type(child(l=15)), pointer :: c3

    !! assign the values to the data components
    c1%ids = (/(i, i=1, 10)/)
    allocate (c1%cx, source=(1.4e0, 3.1e0))

    c2%ids = (/(10*j, j=int4bound, int4bound+4)/)
    allocate (c2%data(0:2), source=(/1.56d0, 2.55d0, 5.12d0/))

    allocate (c3)

    c3%ids = (/(i, i=15,1,-1)/)
    c3%data => r1
    allocate (c3%cx, source=(1.0_4, 2.1_4))


    !! verify the results
    if (associated (c1%data)) error stop 1_4
    if (any(c1%ids /= (/(i, i=1, 10)/))) error stop 2_4
    if (.not. precision_x8(c1%cx, (1.4e0, 3.1e0))) error stop 3_4


    if (allocated(c2%cx)) error stop 4_4
    if ((.not. precision_r8(c2%data(0), 1.56d0)) .or. &
        (.not. precision_r8(c2%data(1), 2.55d0)) .or. &
        (.not. precision_r8(c2%data(2), 5.12d0)))   error stop 5_4

    if (any (c2%ids/int4bound /= 10)) error stop 6_4


    if (.not. associated (c3%data, r1)) error stop 7_4
    if (any(c3%ids(15:1:-1) /= (/(i, i=1,15)/))) error stop 8_4
    if (.not. precision_x8 (c3%cx, (1.0_4, 2.1_4))) error stop 9_4
end subroutine

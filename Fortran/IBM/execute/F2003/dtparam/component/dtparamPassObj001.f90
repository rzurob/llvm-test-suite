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
!*  DATE                       : 01/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.3: the passed-object
!                               dummy-arg)
!                               Case: Test the overriding binding with the same
!                               corresponding kind type parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        integer(k) :: id

        contains

        procedure :: print4 => printBase4
        procedure :: print8 => printBase8
    end type

    type, extends(base) :: child
        real(k) :: data

        contains

        procedure :: print4 => printChild4
        procedure :: print8 => printChild8
    end type

    contains

    subroutine printBase4 (b)
        class(base(4)), intent(in) :: b

        write(*, '(i10)') b%id
    end subroutine

    subroutine printBase8 (b)
        class(base(8)), intent(in) :: b

        write(*, '(i20)') b%id
    end subroutine

    subroutine printChild4 (b)
        class(child(4)), intent(in) :: b

        write (*, '(i10,f10.2)') b%id, b%data
    end subroutine

    subroutine printChild8 (b)
        class(child(8)), intent(in) :: b

        write (*, '(i20,f15.5)') b%id, b%data
    end subroutine
end module

program dtparamPassObj001
use m
    class(base), pointer :: b1(:)
    class(base(8)), allocatable :: b2, b3
    class (base(2)), allocatable :: b4      !<-- ensure this is still legal

    allocate (b1(10), source=(/(child(i, i*1.0e1), i=1,10)/))
    allocate (b2, source=child(8)(2_8**35, dcos(1.0d0)))

    allocate (b3, b4)

    b3%id = -100
    b4%id = -1

    do i = 1, 10
        call b1(i)%print4
    end do

    call b2%print8()
    call b3%print8

    print *, b4%id
end

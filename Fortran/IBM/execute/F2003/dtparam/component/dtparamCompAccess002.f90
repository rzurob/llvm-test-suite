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
!*  DATE                       : 01/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Derived type parameter and private access
!                               for component: use of generic interface to
!                               override the structure constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        complex, private :: cx(0:n) = (0.0, 0.0)

        contains

        procedure :: print => printBase
    end type

    interface base
        module procedure genBaseAlloc
    end interface

    contains

    subroutine printBase(b)
        class(base(*)), intent(in) :: b

        do i = 1, b%n
            write(*, '("(", f10.2, ",", f10.2, ")")') b%cx(i)
        end do

        write (*, '("sum = ", 2g10.3)') b%cx(0)
    end subroutine

    function genBaseAlloc (cx) result (r)
        type(base(:)), allocatable :: r
        complex, intent(in) :: cx(:)

        allocate (base(size(cx)) :: r)

        r%cx(1:) = cx
        r%cx(0) = sum (cx)
    end function
end module

program dtparamCompAccess002
use m
    type (base(:)), allocatable :: b1

    complex cx1(100)

    cx1 = (/(cmplx(i*1.0, i*2.0), i=1,100)/)

    allocate (b1, source=base(cx1(1:10)))

    call b1%print

    deallocate (b1)

    allocate (b1, source= base(cx1(1:50:3)))

    call b1%print
end

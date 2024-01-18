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
!*  DATE                       : 01/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Assumed type parameter in
!                               argument-association: in module procedure.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        complex(8) :: cx(n)
    end type

    contains

    subroutine printBase (b)
        type (base(*)), intent(in) :: b

        do i = 1, b%n
            write (*, '("(", g15.5, ",", g15.5, ")")') b%cx(i)
        end do
    end subroutine

    subroutine setBase (b, cx)
        type (base(*)), intent(out) :: b
        complex(8), intent(in) :: cx(b%n)

        b%cx = cx
    end subroutine
end module

program assumedparam001
use m
    type(base(10)) b1
    type (base(:)), allocatable :: b2
    complex(8) cx1(100)

    b1%cx = (/(cmplx(i*1.d0, (i+1)*1.d0, 8), i = 1, 10)/)

    cx1 = (/(1.d39*cmplx(i*1.d0, i*2.d0, 8), i = 1, 100)/)

    call printBase (b1)

    allocate (base(15):: b2)

    call setBase(b2, cx1(2))

    call printBase(b2)

    deallocate (b2)

    allocate (base(20):: b2)

    call setBase (b2, cx1(10))

    call printBase(b2)
end

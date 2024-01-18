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
!*  DATE                       : 12/13/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Empty type with type parameters and type
!                               bounds; and type extension.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer(8), kind :: k = 8

        contains

        procedure :: print4 => printBase4
        procedure :: print8 => printBase8

        generic :: print => print4, print8
    end type

    contains

    subroutine printBase4 (b)
        class (base(k=4)), intent(in) :: b

        print *, 'printBase4'
    end subroutine

    subroutine printBase8 (b)
        class (base), intent(in) :: b

        print *, 'printBase8'
    end subroutine
end module

module m1
use m
    type, extends(base) :: child

        real(kind=k) data
        integer(kind=k) id

        contains

        procedure :: print4 => printChild4
        procedure :: print8 => printChild8
    end type

    contains

    subroutine printChild4 (b)
        class (child(4)), intent(in) :: b

        print *, 'printChild4'
        write (*, '(f10.2, i10)') b%data, b%id
    end subroutine

    subroutine printChild8 (b)
        class (child(k=8)), intent(in) :: b

        print *, 'printChild8'
        write (*, '(f15.5, i10)') b%data, b%id
    end subroutine
end module

program dtparamExtends024
use m1
    type (child(8)) c1(2)
    type (child(4)) c2

    c1 = (/child(8)(2.1d0, -100_8), child(8)(-1.54d1, 100_8)/)
    c2 = child(4) (3.2e0, -1)

    call c1(1)%print
    call c1(2)%print
    call c2%print

    call c1(1)%base%print
    call c1(2)%base%print
    call c2%base%print
end

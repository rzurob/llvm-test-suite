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
!*  DATE                       : 05/17/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A simple test to verify
!                               that  two bindings distibnguished by kind type
!                               parameter can be used in the same derived type.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer nval
        real(k), pointer :: val(:)

        contains

        procedure :: print4 => printBase4
        procedure :: print8 => printBase8
        generic :: print => print4, print8
    end type

    contains

    subroutine printBase4 (b1)
        class (base(4)), intent(in) :: b1

        print *, 'in print4'

        if (associated(b1%val)) then
            print *, b1%nval, b1%val
        else
            print *, b1%nval, 'NULL'
        end if
    end subroutine

    subroutine printBase8 (b1)
        class (base(8)), intent(in) :: b1

        print *, 'in print8'

        if (associated(b1%val)) then
            print *, b1%nval, b1%val
        else
            print *, b1%nval, 'NULL'
        end if
    end subroutine
end module

use m
    type(base(4)) b1
    type(base(8)), allocatable :: b2

    b1 = base(4)(100, null())

    allocate (b2, source= base(8)(2, null()))

    call b1%print4
    call b2%print8

    allocate (b1%val(b1%nval), b2%val(b2%nval))

    b1%val = [(j, j=100,1,-1)]
    b2%val = [2, 1]

    call b1%print4
    call b2%print8

    call b2%print
    call b1%print
end

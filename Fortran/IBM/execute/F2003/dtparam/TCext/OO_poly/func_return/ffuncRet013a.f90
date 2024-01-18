! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet013a.f
! opt variations: -qnol -qnodeferredlp

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
!*  DATE                       : 05/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (recursive function)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: data = 0
    end type

    contains

    recursive class(base(:,8)) function factorial (n)
        allocatable factorial
        integer, intent(in) :: n

        integer(8) :: seed = 1

        if (n <= 1) then
            allocate (factorial, source=base(20,8)(1))
            seed = 1
        else
            associate (x => factorial(n-1))
                seed = seed + 1
                allocate (factorial, source=base(20,8)(seed*x%data))
            end associate
        end if
    end function
end module

program ffuncRet013a
use m
    class (base(:,8)), allocatable :: b1

    allocate (b1, source=factorial(3))

    if (b1%data /= 6) error stop 1_4

    associate (x => factorial(10))
        if (x%data /= 3628800) error stop 2_4
    end associate

    select type (x => factorial(12))
        type is (base(*,8))
            if (x%data /= 479001600_8) error stop 3_4
        class default
            error stop 4_4
    end select
end

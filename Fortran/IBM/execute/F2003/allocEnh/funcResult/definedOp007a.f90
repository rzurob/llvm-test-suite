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
!*  DATE                       : 09/26/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               An similar case to definedOp007, except the
!                               component is of array.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(:), allocatable :: str(:)

        contains

        procedure :: concat => concatBaseStr
        generic :: operator(//) => concat
    end type

    contains

    character(:) function concatBaseStr (b, c)
        class(base), intent(in) :: b
        character(*), intent(in) :: c

        allocatable concatBaseStr(:)

        if (allocated(b%str)) then
            concatBaseStr = b%str // c
        else
            concatBaseStr = [c]
        end if
    end function
end module

program definedOp007a
use m
    type(base), allocatable :: b1(:)

    b1 = [base(null()), base(null())]

    b1(1) = base(b1(1) // 'xlftest')

    b1(2) = base([b1(1)%str, 'tsetflx'])

    b1(2) = base(b1(2)%str // ' 101')

    print *, b1(1)%str, '|', b1(2)%str
end

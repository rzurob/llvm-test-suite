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
!*  DATE                       : 12/30/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used as length for
!                               character component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (len, n)
        integer, kind :: len, n

        character(len) :: names(n)
    end type
end module


program kindparamSpecexpr001
use m
    type(base(30, 3)) b1
    type (base(20, 9)) b2(10)

    b1%names = (/('len=30, n=3; element: '//char(ichar('0')+i), i=1,3)/)
    b2(2)%names = (/('20:9; element: '//char(ichar('0')+i), i=1,9)/)

    print *, b1
    print *, b2(2)
end

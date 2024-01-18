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
!*  DATE                       : 03/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: diagnostic case: Use logical to assign to
!                               numerical and character values.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n, l)
        integer, kind :: k
        integer, len :: n, l

        logical(k/4) :: flags(n)
        character(l) :: name
        real(k) :: data(n)
    end type
end module

program dtparamConstr030d
use m
    type (base(8, 10, 11)) :: b1

    logical(8) :: l1(10)

    b1 = base(8,10,11) (1.0, 'xlftest', data=1)
    b1 = base(8,10,11) (l1, 'xlftest', data=l1)
    b1 = base(8,10,11) (l1, data=1, name=l1(1))
end

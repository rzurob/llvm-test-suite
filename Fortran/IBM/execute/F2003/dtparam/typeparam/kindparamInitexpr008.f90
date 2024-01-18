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
!*  DATE                       : 12/27/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used for the default
!                               initializations for other type parameters.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (init, k1, n, l)
        integer, kind :: init = 10
        integer, kind :: k1 = max(4, mod(init, 5))
        integer, len :: n=init, l = init+k1

        integer(k1) :: id(n)
        character(l) :: name
    end type
end module

program kindparamInitexpr008
use m
    type(base) b1
    type(base(24)) b2(10)
    type(base(11, k1=8)) b3

    !! verify the parameter values
    if ((kind(b1%id) /= 4) .or. (size(b1%id) /= 10)) error stop 1_4
    if (len(b1%name) /= 14) error stop 2_4

    if ((b2%k1 /= 4) .or. (b2%n /= 24) .or. (len (b2%name) /= 28)) &
            error stop 3_4

    b3%id = (/(i+2_8**33, i=1,11)/)
    b3%name = 'xlftest F2003 test team'

    print *, b3
end

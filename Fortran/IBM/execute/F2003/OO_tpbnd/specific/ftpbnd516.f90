!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd516.f
! %VERIFY: ftpbnd516.out:ftpbnd516.vf
! %STDIN:
! %STDOUT: ftpbnd516.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (function that returns
!*                               allocatable array)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: makeArray => createBaseArray
    end type

    contains

    function createBaseArray (b, n)
        class (base), intent(in) :: b
        type (base), allocatable :: createBaseArray (:)
        integer*4, intent(in) :: n

        allocate (createBaseArray(n))
        createBaseArray = base(b%id)
    end function
end module

program ftpbnd516
use m
    type (base) :: b1, b2 (10)

    b1%id = 10

    print *, b1%makeArray(2)

    b1%id = 2
    b2 = b1%makeArray(10)

    print *, b2

    if (any (b2%id /= 2)) error stop 1_4
end

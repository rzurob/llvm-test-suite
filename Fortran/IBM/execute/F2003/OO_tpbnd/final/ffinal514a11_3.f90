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
! %GROUP: ffinal514a11_3.f
! %VERIFY: ffinal514a11_3.out:ffinal514a11_3.vf
! %STDIN:
! %STDOUT: ffinal514a11_3.out
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
!*  DATE                       : 06/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : final sub (finalization of function returned
!                               temps in WHERE statement/construct)
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
        integer*4 :: id = 1

        contains

        final :: finalizeBase
    end type

    interface makeData
        function makeBase (i)
        import base
            type(base) :: makeBase
            integer*4, intent(in) :: i
        end function
    end interface

    interface operator (==)
        elemental logical function baseEqual (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine
end module


program ffinal514a11_3
use m
    type (base), save :: b1(10)

    b1%id = (/2,3,5,5,6,7,7,7,9,10/)

    where (b1 == makeData (7))
        b1%id = -b1%id
    end where

    print *, 'second where'

    where (b1 == makeData (5)) b1%id = 105

    if (any(b1%id /= (/2,3,105,105,6,-7,-7,-7,9,10/))) error stop 1_4

    print *, 'end'
end

function makeBase (i)
use m, only: base
    type(base) :: makeBase
    intent(in) i

    makeBase%id = i
end function

elemental logical function baseEqual (b1, b2)
use m, only:base
    type (base), intent(in) :: b1, b2

    baseEqual = (b1%id == b2%id)
end function

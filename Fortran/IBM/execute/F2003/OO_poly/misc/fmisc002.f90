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
! %GROUP: fmisc002.f
! %VERIFY: fmisc002.out:fmisc002.vf
! %STDIN:
! %STDOUT: fmisc002.out
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
!*  DATE                       : 05/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : miscellaneous items (defect 283383)
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
        integer*4 :: id = 0
    end type
end module

program fmisc002
use m
    interface
        function makeBaseArray (id, n)
        use m
            integer*4, intent(in) :: id, n
            type (base) :: makeBaseArray (n)
        end function
    end interface

    type (base) :: b1(2)

    b1 = makeBaseArray (10, 2)
    print *, b1
    print *, makeBaseArray (10, 2)
end

function makeBaseArray (id, n)
use m
    integer*4, intent(in) :: id, n
    type (base) :: makeBaseArray (n)

    makeBaseArray%id = id
end function


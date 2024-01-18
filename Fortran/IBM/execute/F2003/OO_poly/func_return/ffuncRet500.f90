!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet500.f
! %VERIFY: ffuncRet500.out:ffuncRet500.vf
! %STDIN:
! %STDOUT: ffuncRet500.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (IMPORT statement usage)
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

        final :: finalizeBase
    end type

    interface makeData
        type (base) function makeBaseObj (i)
        import base
            integer*4, intent(in) :: i
        end function
    end interface

    contains

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program ffuncRet500
use m
    print *, makeData (10)
    print *, 'end'
end

function makeBaseObj (i)
use m, only : base
    implicit type (base) (m)
    integer*4, intent(in) :: i

    makeBaseObj%id = i
end function


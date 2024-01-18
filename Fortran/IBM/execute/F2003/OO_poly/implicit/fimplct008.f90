!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct008.f
! %VERIFY: fimplct008.out:fimplct008.vf
! %STDIN:
! %STDOUT: fimplct008.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implicit with import statement;
!*                               basics)
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
    end type

    interface makeData
        function makeBaseObj (i)
        import base
            implicit type (base) (m)
            integer*4, intent(in) :: i
        end function
    end interface
end module

program fimplct008
use m
    print *, makeData (10)
end

function makeBaseObj (i)
use m, only : base
    implicit type (base) (m)
    integer*4, intent(in) :: i

    makeBaseObj%id = i
end function

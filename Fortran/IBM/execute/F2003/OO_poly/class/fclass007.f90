!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass007.f
! %VERIFY: fclass007.out:fclass007.vf
! %STDIN:
! %STDOUT: fclass007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly-pointer function
!                               result used as the actual argument)
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
    contains

    class (*) function makeData (x)
        class (*), intent(in) :: x

        pointer makeData

        allocate (makeData, source=x)
    end function

    subroutine test1 (x)
        class (*), pointer, intent(in) :: x

        select type (x)
            type is (integer)
                print *, x
            type is (real)
                write (*, '(f10.2)') x
            type is (character(*))
                print *, x
        end select
    end subroutine
end module

program fclass007
use m
    call test1 (makeData (100))

    call test1 (makeData (12.3))

    call test1 (makeData ('xlftest team'))
end

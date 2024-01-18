!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass009.f
! %VERIFY: fclass009.out:fclass009.vf
! %STDIN:
! %STDOUT: fclass009.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (defined operator usage in
!                               ASSOCIATE construct)
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
        integer(4) :: value
    end type

    interface operator (+)
        function b1AddI (b1, i)
        import base
            type (base), intent(in) :: b1
            integer*4, intent(in) :: i
            type (base) b1AddI
        end function

        function iAddB1 (i, b1)
        import base
            integer*4, intent(in) :: i
            type (base), intent(in) :: b1
            type (base) iAddB1
        end function
    end interface
end module

program fclass009
use m
    type (base) :: b1

    b1%value = 10

    associate (x => (1+b1), x1 => (b1 + 100))
        print *, x%value, x
        print *, x1, x1%value

        print *, 1 + x, x+10
        print *, 10+x1, x1+1
    end associate
end

type (base) function b1AddI (b1, i)
use m, only: base
    type (base), intent(in) :: b1
    integer*4, intent(in) :: i

    b1AddI%value = b1%value + i
end function

type (base) function iAddB1 (i, b1)
use m, only: base
    integer*4, intent(in) :: i
    type (base), intent(in) :: b1

    iAddB1%value = b1%value + i
end function

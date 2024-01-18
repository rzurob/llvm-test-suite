!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal514a2.f
! %VERIFY: ffinal514a2.out:ffinal514a2.vf
! %STDIN:
! %STDOUT: ffinal514a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!*                               by function calls in an array constructor)
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

        procedure :: replicate => produceBase

        final :: finalizeBase
    end type

    type (base) :: b1_m(3), b2_m(3)

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        b%id = 0
    end subroutine

    type (base) function produceBase (b)
        class (base), intent(in) :: b

        produceBase%id = b%id
    end function
end module

program ffinal514a2
use m
    b2_m%id = (/1, 2, 3/)

    b1_m = (/(b2_m(i)%replicate(), i=1,3)/)

    print *, 'end'

    if (any (b1_m%id /= (/1, 2, 3/))) error stop 1_4
end

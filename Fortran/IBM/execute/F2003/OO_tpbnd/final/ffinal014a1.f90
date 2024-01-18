!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal014a1.f
! %VERIFY: ffinal014a1.out:ffinal014a1.vf
! %STDIN:
! %STDOUT: ffinal014a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (poly-pointer's deallocation)
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

    type, extends(base) :: child
        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child) :: c
        print *, 'finalizeChild', c%id
    end subroutine
end module

program ffinal014a1
use m
    type (child), pointer :: c_ptr

    class (base), pointer :: b

    allocate (c_ptr)

    c_ptr%id = 10

    b => c_ptr

    deallocate (b)

end

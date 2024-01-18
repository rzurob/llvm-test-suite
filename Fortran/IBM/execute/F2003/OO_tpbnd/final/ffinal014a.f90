!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal014a.f
! %VERIFY: ffinal014a.out:ffinal014a.vf
! %STDIN:
! %STDOUT: ffinal014a.out
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
!*  DESCRIPTION                : final sub (poly-pointer's finalization)
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

    type, extends(base) :: child
        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child) :: c
        print *, 'finalizeChild', c%id
    end subroutine
end module

program ffinal014a
use m
    type (base), pointer :: b_ptr
    type (child), pointer :: c_ptr

    class (base), pointer :: b

    allocate (b_ptr, c_ptr)

    b_ptr%id = 1
    c_ptr%id = 10

    b => c_ptr

    deallocate (b)

    print *, 're-assign pointer b'

    b => b_ptr

    deallocate (b)
end

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
! %GROUP: fArg510a1.f
! %VERIFY: fArg510a1.out:fArg510a1.vf
! %STDIN:
! %STDOUT: fArg510a1.out
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
!*  DATE                       : 04/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : dummy-arg (defined assignment using
!*                               poly-dummy-arg; interface defined in module)
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
        integer*4 id
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    private base2Base, child2Child

    interface assignment (=)
        subroutine base2Base (b1, b2)
        import base
            class (base), intent(out) :: b1
            type (base), intent(in) :: b2
        end subroutine

        subroutine child2Child (c1, c2)
        import child
            class (child), intent(out) :: c1
            type (child), intent(in) :: c2
        end subroutine
    end interface
end module

subroutine base2Base (b1, b2)
use m
    class (base), intent(out) :: b1
    type (base), intent(in) :: b2

    print *, 'defined assignment for base'

    b1%id = b2%id
end subroutine

subroutine child2Child (c1, c2)
use m
    class (child), intent(out) :: c1
    type (child), intent(in) :: c2

    c1%base = c2%base
    c1%name = c2%name
end subroutine


program fArg510a1
use m
    type (child) :: c1, c2

    class (base), allocatable :: b_allo

    c1 = child (100, 'c1, maybe c2')

    c2 = c1

    if ((c2%id /= 100) .or. (c2%name /= 'c1, maybe c2')) error stop 1_4

    allocate (b_allo)

    b_allo = c1%base

    if (b_allo%id /= 100) error stop 2_4
end

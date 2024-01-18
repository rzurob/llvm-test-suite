!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr041a.f
! %VERIFY: fconstr041a.out:fconstr041a.vf
! %STDIN:
! %STDOUT: fconstr041a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (for entities from
!*                               IMPLICIT statement)
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
    implicit type(child) (c)
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    data c1_m / child(base = base(id=1), name = 'c1_m') /

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fconstr041a
use m, only : child, c1_m
    IMPLICIT type (child) (c)

    c1 = child (10, 'c1')

    call c1%print

    call c1_m%print

    if ((c1_m%id /= 1) .or. (c1_m%name /='c1_m')) error stop 1_4

    if ((c1%id /= 10) .or. (c1%name /='c1')) error stop 2_4
end

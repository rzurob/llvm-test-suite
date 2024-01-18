!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr050.f
! %VERIFY: fconstr050.out:fconstr050.vf
! %STDIN:
! %STDOUT: fconstr050.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (non-poly allocatable
!                               component with poly-entites as the data-source)
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
        integer(4) id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) name

        contains

        procedure :: print => printChild
    end type

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

module m1
use m
    type container
        type (base), allocatable :: data
    end type
end module

program fconstr050
use m1
    type (container) :: co1
    class (base), allocatable :: b1

    allocate (b1, source=child(1, 'test'))

    call b1%print

    !! in an associate construct
    associate ( x => container (b1))
        print *, x%data
        call x%data%print
    end associate

    !! involve an intrinsic assignment
    co1 = container (b1)

    call co1%data%print
end


!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fimplct005.f
! %VERIFY: fimplct005.out:fimplct005.vf
! %STDIN:
! %STDOUT: fimplct005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implicit used in type bound
!*                               declaration)
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

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
    implicit class (base) (b)
        intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
    implicit class (child) (b)
        intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct005
use m
    implicit class (base) (b), class (child) (c)

    pointer b1, c1

    allocate (b1)
    allocate (c1)

    b1%id = 1

    c1%id = 10
    c1%name = 'c1'

    call b1%print
    call c1%print

    deallocate (b1)

    b1 => c1

    call b1%print

    deallocate (b1)
end

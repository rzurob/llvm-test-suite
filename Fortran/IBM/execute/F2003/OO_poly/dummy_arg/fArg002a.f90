!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg002a.f
! %VERIFY: fArg002a.out:fArg002a.vf
! %STDIN:
! %STDOUT: fArg002a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array constructor
!*                               containing poly-entities used as actual
!*                               argument)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'

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
    end subroutine printChild
end module

module m1
use m
    contains

    subroutine printData (d)
        class (base), intent(in) :: d(:)

        do i = 1, size (d)
            call d(i)%print
        end do
    end subroutine
end module

program fArg002a
use m1
    class (base), allocatable :: b1, b2

    allocate (b1, source=child(1,'test1'))
    allocate (b2, source=child(2,'test2'))

    call printData (d = (/b1, b2/))
end

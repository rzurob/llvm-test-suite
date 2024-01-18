!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fclass002a.f
! %VERIFY: fclass002a.out:fclass002a.vf
! %STDIN:
! %STDOUT: fclass002a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class (argument association,
!*                               poly-dummy-arg-array; resolve pass type-bounds)
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
        integer*4 :: id = 0

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = ''

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

program fclass002a
use m

    type (base) :: b1(2:4)
    type (child) :: c1(0:3)

    b1 = (/(base (i*10), i=2,4)/)

    c1 = (/(child(i+1, name='c1'), i=0,3)/)

    call abc (b1)

    call abc (c1)

    contains

    subroutine abc (a)
        class (base), intent(in) :: a(:)

        do i = 1, size(a)
            call a(i)%print
        end do
    end subroutine
end

!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp507.f
! %VERIFY: fselTyp507.out:fselTyp507.vf
! %STDIN:
! %STDOUT: fselTyp507.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/1/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (deferred binding call in select
!                               type construct where selector is an abstract
!                               type by declaration)
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
    type, abstract :: base
        integer id

        contains

        procedure (print1), pass (b), deferred :: print
    end type

    type, extends (base) :: child
        character (20) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine print1(b)
        import base
            class (base), intent(in) :: b
        end subroutine
    end interface

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fselTyp507
use m
    class (base), pointer :: b1

    allocate (b1, source=child(10, 'b1'))

    select type (b1)
        class is (base)
            call b1%print
        class default
            print *, 'wrorng'
    end select
end

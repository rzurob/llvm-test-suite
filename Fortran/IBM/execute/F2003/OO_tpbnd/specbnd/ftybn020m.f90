!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn020m.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn020m.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass, non_overridable
!*
!*  DESCRIPTION                : the accessiblity of a type-bound procedure
!*                               is not affected by a PRIVATE statement
!*                               in the component-part, the accessiblity
!*                               of a data component is not affected by a
!*                               PRIVATE statemnt in the type-bound-procedure
!*                               -part.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, private :: id = 10
    end type

    type, extends(base) :: child
        character(10) :: name
    contains
        procedure , nopass :: bind => verify
    end type

    type (child), save :: c2

    contains
        logical function verify(c, int, char)
            type(child), intent (in) :: c
            integer, intent (in) :: int
            character(*), intent (in) :: char

            verify = ((c%base%id .eq. int) .and. (c%name .eq. char))
        end function
end module

program fext002
    use m
    type (child) :: c1

    c1%name = 'c1'
    c2%name = 'c2'

    if (.not. c1%bind(c1, 10, 'c1')) error stop 1_4

    if (.not. c2%bind(c2, 10, 'c2')) error stop 2_4
end

! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn020n.f
! opt variations: -qck -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn020n.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn020n.f
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
    type, private :: base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), private :: id = 10
    end type

    type, extends(base) :: child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: name
    contains
        procedure , nopass :: bind => verify
    end type

    type (child(4,10)), save :: c2

    contains
        logical function verify(c, int, char)
            type(child(4,*)), intent (in) :: c
            integer, intent (in) :: int
            character(*), intent (in) :: char

            verify = ((c%base%id .eq. int) .and. (c%name .eq. char))
        end function
end module

    use m
    type (child(4,10)) :: c1

    c1%name = 'c1'
    c2%name = 'c2'

    if (.not. c1%bind(c1, 10, 'c1')) error stop 1_4

    if (.not. c2%bind(c2, 10, 'c2')) error stop 2_4
end

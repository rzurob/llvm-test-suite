!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext038.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (base type name is the same as
!*                               that of a component)
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
    type base(k1)
        integer, kind :: k1
        integer(k1), private :: base = 1
    end type

    type (base(4)) :: b1_m = base(4)(base = 10)
    type (base(4)), save :: b2_m
    contains

    subroutine setBase (b, intVal)
        type (base(4)), intent(inout) :: b
        integer*4, intent(in) :: intVal

        b%base = intVal
    end subroutine

    integer*4 function getValue (b)
        type(base(4)), intent(in) :: b

        getValue = b%base
    end function
end module

program fext038
use m

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (child(4,20)) :: c1, c2
    type (child(4,20)) :: c3

    c2 = child (4,20)(name = 'c2')
    c3 = child (4,20)(base = b1_m, name = 'c3')

    c1%name = 'c1'

    call setBase (c1%base, 10)

    if (getValue(c1%base) /= 10) error stop 1_4
    if (c1%name /= 'c1') error stop 2_4

    if (getValue(c2%base) /= 1) error stop 3_4
    if (c2%name /= 'c2') error stop 4_4

    if (getValue(c3%base) /= 10) error stop 5_4
    if (c3%name /= 'c3') error stop 6_4

    if (getValue(b2_m) /= 1) error stop 7_4
    if (getValue(b1_m) /= 10) error stop 8_4
end

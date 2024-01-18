!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (name conflict is
!*                               allowed for unaccessible parent's component)
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
        integer*4 :: id
        real*4, private :: value
    end type

    contains

    subroutine setValue (b, realVal)
        type (base), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base), intent(in) :: b

        getValue = b%value
    end function
end module

module m1
use m
    type, extends(base) :: child
        integer(8) :: value   ! name conflict is allowed here
        character(20) :: name
    end type
end module

program fext012
    use m1
    type (child) :: c1
    type (base) :: b1

    call setValue (b1, 1.0)
    b1%id = 1

    c1%id = 20
    c1%name = 'This is a test'
    c1%value = 100
    call setValue(c1%base, -10.0)

    if (c1%base%id /= 20) error stop 1_4
    if (c1%name /= 'This is a test') error stop 2_4
    if (c1%value /= 100) error stop 3_4
    if (getValue(c1%base) /= -10.0) error stop 4_4

    if (getValue(b1) /= 1.0) error stop 5_4
    if (b1%id /= 1) error stop 6_4
end

!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext025.f
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
!*  DESCRIPTION                : an extended type introduced via use and only
!*                               the extended type is exposed
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
    type base(ki,kv)
        integer, kind :: ki,kv
        integer(ki) :: id
        real(kv), private :: value
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(20) :: name
    end type

    type (base(4,4)) :: b1_m
    type (child(4,4,20)) :: c1_m

    contains

    subroutine setValue (b, realVal)
        type (base(4,4)), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base(4,4)), intent(in) :: b

        getValue = b%value
    end function

end module

program fext025
    use m, only: child, setValue, getValue, b1_m, c1_m

    type (child(4,4,20)) :: c1

    call setValue (b1_m, 15.0)
    call setValue (c1_m%base, 20.0)

    b1_m%id = 10
    c1_m%id = 20
    c1_m%name = 'c1_m'

    call setValue (c1%base, 10.0)
    c1%id = 100
    c1%name = 'data c1'

    if (c1%base%id /= 100) error stop 1_4
    if (c1%name /= 'data c1') error stop 2_4
    if (getValue(c1%base) /= 10.0) error stop 3_4

    if (b1_m%id /= 10) error stop 4_4
    if (getValue(b1_m) /= 15.0) error stop 5_4

    if (c1_m%id /= 20) error stop 6_4
    if (c1_m%name /= 'c1_m') error stop 7_4
    if (getValue(c1_m%base) /= 20.0) error stop 8_4

    if (c1_m%base%id /= 20) error stop 9_4
end

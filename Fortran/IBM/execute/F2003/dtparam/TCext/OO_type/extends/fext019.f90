!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext019.f
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
!*  DESCRIPTION                : derived-type extension (base type is renamed
!*                               via use association. Test accessibilty of the
!*                               inherited module procedures)
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

    contains

    subroutine setValue(b, realVal)
        type(base(4,4)), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue(b)
        type(base(4,4)), intent(in) :: b

        getValue = b%value
    end function
end module

module m1
use m, newBase => base

    type, extends(newBase) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (newBase(4,4)) :: b1_m
    type (child(4,4,20)) :: c1_m

    contains

    subroutine initializeB1_m
        b1_m%id = 100
        call setValue (b1_m, -1.0)
    end subroutine

    subroutine initializeC1_m
        c1_m%newbase%id = 10
        c1_m%name = 'module data'
        call setValue(c1_m%newbase, 1.0)
    end subroutine
end module

program fext019
    use m1

    type (newBase(4,4)) :: b1
    type (child(4,4,20)) :: c1

    call initializeB1_m
    call initializeC1_m

    if (b1_m%id /= 100) error stop 1_4
    if (getValue(b1_m) /= -1.0) error stop 2_4

    if (c1_m%newbase%id /= 10) error stop 3_4
    if (c1_m%name /= 'module data') error stop 4_4
    if (getValue(c1_m%newbase) /= 1.0) error stop 5_4

    b1%id = 1000
    call setValue (b1, 10.0)
    if (b1%id /=1000) error stop 6_4
    if (getValue(b1) /= 10.0) error stop 7_4

    c1%id = 10000
    c1%name = 'main data c1'
    call setValue(c1%newbase, 2.0)
    if (c1%newbase%id /= 10000) error stop 8_4
    if (c1%name /= 'main data c1') error stop 9_4
    if (getValue(c1%newbase) /= 2.0) error stop 10_4
end

!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext007.f
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
!*  DESCRIPTION                : reference of parent components via full name
!*                               or short-hand name.  Parent/extended in the
!*                               different scoping units
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
        integer, kind :: ki, kv
        integer(ki) :: id
        real(kv), private :: value
    end type
    contains

    subroutine setValue (b, realVal)
        type (base(4,4)), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base(4,4)), intent (in) :: b

        getValue = b%value
    end function
end module

module m1
use m
    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (child(4,4,20)) :: c1_m

    contains

    subroutine initializeC1_m
        c1_m%id = 10
        call setValue(c1_m%base, 10.0)
        c1_m%name = 'module data c1_m'
    end subroutine
end module

program fext007
    use m1
    type (child(4,4,20)) :: c1

    c1%id = 20  ! short-hand name
    c1%name = 'This is a test'
    call setValue(c1%base, 1.0)

    if ( c1%base%id /= 20) error stop 1_4  ! full name reference
    if ( c1%name /= 'This is a test') error stop 2_4
    if ( getValue(c1%base) /= 1.0 ) error stop 3_4

    call initializeC1_m

    if (c1_m%id /= 10) error stop 4_4   ! short-hand name
    if (c1_m%base%id /= 10) error stop 5_4   ! full name
    if (c1_m%name /= 'module data c1_m') error stop 6_4
    if (getValue(c1_m%base) /= 10.0) error stop 7_4
end

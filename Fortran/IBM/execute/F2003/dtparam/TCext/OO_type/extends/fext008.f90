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
!*                               same scoping unit, private component without
!*                               default initialization.
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
        integer, kind :: ki=4, kv=4
        integer(ki) :: id
        real(kv), private :: value
    end type

    type, extends(base) :: child(n)
        integer, len :: n=20
        character(n) :: name
    end type

    type (child) :: c1_m

    contains

    ! in this subroutine parent components accessed via short-hand name
    subroutine initializeC1_m
        c1_m%id = 10
        c1_m%value = 10.0
        c1_m%name = 'module data c1_m'
    end subroutine

    ! in this function parent component value accessed using full name
    real*4 function getValue (c)
        type (child(kv=4)), intent (in) :: c

        getValue = c%base%value
    end function

    ! in this function parent component value accessed using short-hand name
    subroutine setValue (c, realVal)
        type (child(n=20,kv=4)), intent (inout) :: c
        real*4, intent(in) :: realVal

        c%value = realVal
    end subroutine
end module

program fext008
    use m
    type (child(kv=4,n=20,ki=4)) :: c1

    c1%base%id = 20   ! full name
    c1%name = 'This is a test'
    call setValue (c1, 1.0)

    if ( c1%id /= 20) error stop 1_4  ! short-hand name
    if ( c1%name /= 'This is a test') error stop 2_4
    if ( getValue(c1) /= 1.0 ) error stop 3_4

    call initializeC1_m

    if (c1_m%id /= 10) error stop 4_4 ! short-hand name
    if (c1_m%name /= 'module data c1_m') error stop 5_4
    if (getValue(c1_m) /= 10.0) error stop 6_4
end

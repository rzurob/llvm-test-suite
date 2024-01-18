!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext006.f
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
!*                               same scoping unit
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
        real*4, private :: value = 1.0
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type (child), save :: c1_m

    contains

    subroutine initializeC1_m
        c1_m%id = 10    ! short-hand reference
        c1_m%value = 10.0    ! short-hand reference
        c1_m%name = 'module data c1_m'
    end subroutine

    real*4 function getValue (c)
        type (child), intent (in) :: c

        getValue = c%value
    end function
end module

program fext006
    use m
    type (child) :: c1

    c1%base%id = 20    ! full name reference
    c1%name = 'This is a test'

    if ( c1%id /= 20) error stop 1_4    ! short-hand reference
    if ( c1%name /= 'This is a test') error stop 2_4
    if ( getValue(c1) /= 1.0 ) error stop 3_4

    call initializeC1_m

    if (c1_m%base%id /= 10) error stop 4_4   ! full name
    if (c1_m%name /= 'module data c1_m') error stop 5_4
    if (getValue(c1_m) /= 10.0) error stop 6_4
end

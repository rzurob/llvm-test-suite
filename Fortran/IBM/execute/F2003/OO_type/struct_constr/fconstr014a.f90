!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr014a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 14, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (derived type with
!*                               private component)
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
        integer*4 ::id
        real*4, private :: value = 10.0
    end type

    type, extends (base) :: child
        character(20) :: name
        logical*2, private :: isSet = .false.
    end type

    type (child), save :: c1_m = child (100, 2.0, 'c1_m')
    type (child), save :: c2_m = child (200, 4.0, 'c2_m', isSet = .true.)

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function

    logical function isChildCorrect (c, intVal, realVal, charVal, logVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal
        logical*2, intent(in) :: logVal

        isChildCorrect = (isBaseCorrect (c%base, intVal, realVal) .and. &
                    (c%name == charVal) .and. (c%isSet .eqv. logVal) )
    end function
end module

program fconstr014a
use m

    type (child) :: c1 = child (id = 1, name = 'c1')
    type (child) :: c2 = child (2, name = 'c2')

    if (.not. isChildCorrect (c1_m, 100, 2.0, 'c1_m', .false._2)) error stop 1_4
    if (.not. isChildCorrect (c2_m, 200, 4.0, 'c2_m', .true._2)) error stop 2_4

    if (.not. isChildCorrect (c1, 1, 10.0, 'c1', .false._2)) error stop 3_4
    if (.not. isChildCorrect (c2, 2, 10.0, 'c2', .false._2)) error stop 4_4
end

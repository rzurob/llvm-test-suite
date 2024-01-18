!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr014.f
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
    end type

    type (child), save :: c1_m = child (100, 2.0, 'c1_m')

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ( (b%id == intVal) .and. (b%value == realVal) )
    end function
end module

program fconstr014
use m

    type (child) :: c1 = child (id = 1, name = 'c1')
    type (child) :: c2 = child (2, name = 'c2')

    if (.not. isBaseCorrect (c1%base, 1, 10.0)) error stop 1_4
    if (c1%name /= 'c1') error stop 2_4

    if (.not. isBaseCorrect (c1_m%base, 100, 2.0)) error stop 3_4
    if (c1_m%name /= 'c1_m') error stop 4_4

    if (.not. isBaseCorrect (c2%base, 2, 10.0)) error stop 5_4
    if (c2%name /= 'c2') error stop 6_4
end

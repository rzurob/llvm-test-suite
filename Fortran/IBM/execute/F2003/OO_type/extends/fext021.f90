!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext021.f
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
!*  DESCRIPTION                : derived-type extension (An extended type
!*                               introduced via use statement and renamed; extended
!*                               type's parent and its parent in procedure calls)
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

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    subroutine setValue(b, realVal)
        type(base), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue(b)
        type(base), intent(in) :: b

        getValue = b%value
    end function

    subroutine initializeChild (c, intVal, realVal, charVal)
        type(child), intent(inout) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        c%id = intVal
        c%name = charVal
        call setValue (c%base, realVal)
    end subroutine
end module

module m1
use m, newBase => child

    ! This newChild is the thirdGeneration
    type, extends(newBase) :: newChild
        logical*2 :: isSet
    end type

    type(newBase) :: c1_m
    type(newChild) :: n1_m

    contains

    subroutine initializeC1_m
        call initializeChild (c1_m, 1, 1.0, 'module data c1_m')
    end subroutine

    subroutine initializeN1_m
        call initializeChild (n1_m%newbase, 2, 2.0, 'module data n1_m')
        n1_m%isSet = .true.
    end subroutine
end module

program fext021
    use m1

    type (newBase) :: c1
    type (newChild) :: n1

    call initializeC1_m

    if (c1_m%id /= 1) error stop 1_4
    if (c1_m%name /= 'module data c1_m') error stop 2_4
    if (getValue(c1_m%base) /= 1.0) error stop 3_4

    if (c1_m%id /= c1_m%base%id) error stop 4_4


    call initializeN1_m

    if (n1_m%id /= 2) error stop 5_4
    if (n1_m%name /= 'module data n1_m') error stop 6_4
    if (.not. n1_m%isSet) error stop 7_4
    if (getValue(n1_m%newbase%base) /= 2.0) error stop 8_4

    if ((n1_m%id /= n1_m%newbase%id) .or. (n1_m%id /= n1_m%base%id) .or. &
        (n1_m%id /= n1_m%newbase%base%id)) error stop 9_4


    call initializeChild (c1, 3, 3.0, 'main data c1')

    if (c1%id /= 3) error stop 10_4
    if (c1%name /= 'main data c1') error stop 11_4
    if (getValue(c1%base) /= 3.0) error stop 12_4

    if (c1%id /= c1%base%id) error stop 13_4

    call initializeChild (n1%newbase, 4, 4.0, 'main data n1')
    n1%isSet = .true.

    if (n1%id /= 4) error stop 14_4
    if (n1%name /= 'main data n1') error stop 15_4
    if (.not. n1%isSet) error stop 16_4
    if (getValue(n1%base) /= 4.0) error stop 17_4

    if ((n1%id /= n1%base%id) .or. (n1%id /= n1%newbase%id) .or. &
        (n1%id /= n1%newbase%base%id)) error stop 18_4
end

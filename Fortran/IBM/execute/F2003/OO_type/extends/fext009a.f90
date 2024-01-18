!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext009a.f
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
!*  DESCRIPTION                : derived-type extension (component inherited,
!*                               parent's components accessed via full name or
!*                               short-hand names.  Multi-generation hiearchy in
!*                               different scoping units.
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
        integer :: id
        real*4, private :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    subroutine setValue (b, realVal)
        type(base), intent(inout) :: b
        real*4, intent (in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type(base), intent(in) :: b

        getValue = b%value
    end function

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type(child), intent(in) :: c
        integer, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id .eq. intVal) .and. (c%value .eq. realVal) .and.&
                        & (c%name .eq. charVal))
    end function
end module

module m1
use m

    type (child) :: c1_m

    type, extends(child) :: thirdGeneration
        logical*2, private :: isSet = .false.
    end type

    type (thirdGeneration), save :: t1_m

    contains

    ! initialization of c1_m without structure constructor
    subroutine initializeC1_m (intVal, realVal, charVal)
        integer, intent (in) :: intVal
        real*4, intent (in) :: realVal
        character(*), intent(in) :: charVal

        ! parent components accessed via short-hand name
        c1_m%id = intVal
        call setValue (c1_m%base, realVal)
        c1_m%name = charVal
    end subroutine

    ! initialization of thirdGeneration without structure constructor
    subroutine initializeThirdGeneration (t, intVal, realVal, charVal, logVal)
        type(thirdGeneration), intent(inout) :: t
        integer, intent (in) :: intVal
        real*4, intent (in) :: realVal
        character(*), intent(in) :: charVal
        logical*2, intent(in) :: logVal

        ! parent components accessed via full name
        t%child%base%id = intVal
        call setValue (t%child%base, realVal)
        t%child%name = charVal
        t%isSet = logVal
    end subroutine

    logical function isThirdGenerationSet (t)
        type (thirdGeneration), intent(in) :: t

        isThirdGenerationSet = t%isSet
    end function
end module

program fext009a
    use m1

    type (thirdGeneration) :: t1

    call initializeC1_m (1, 1.0, 'c1_m')

    call initializeThirdGeneration (t1_m, 2, 10.0, 't1_m', .true._2)

    if (.not. isChildCorrect (c1_m, 1, 1.0, 'c1_m')) error stop 1_4

    if (.not. isChildCorrect (t1_m%child, 2, 10.0, 't1_m')) error stop 2_4

    if (.not. isThirdGenerationSet (t1_m)) error stop 3_4

    call initializeThirdGeneration (t1, 3, 100.0, 't1', .true._2)

    ! check t1 element by element
    if (getValue (t1%base) /= 100.0) error stop 4_4
    if (t1%id /= 3) error stop 5_4
    if (t1%name /= 't1') error stop 6_4
    if (.not. isThirdGenerationSet (t1)) error stop 7_4

    ! verify that short hand and full name are equivalent
    if ((t1%id /= t1%child%id) .or. (t1%id /= t1%base%id) .or. &
        (t1%id /= t1%child%base%id)) error stop 8_4

    if (t1%child%name /= 't1') error stop 9_4
end

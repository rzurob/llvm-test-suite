! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (keyword usage in the
!*                               component-spec; ultimate component only)
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
    end type

    type, extends(base) :: child
        character(20) :: name
        real*4, private :: value = 1.0
    end type

    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type (thirdGeneration), save :: t1_m = thirdGeneration (id = 10, &
            name = 't1_m', isSet=.true.)

    type (child), save :: c1_m = child (name = 'c1_m', id = 20)

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                          .and. (c%name == charVal))
    end function

    subroutine addValue (c, realVal)
        type (child), intent(inout) :: c
        real*4, intent(in) :: realVal

        c%value = c%value + realVal
    end subroutine
end module

program fconstr017a
use m

    type (thirdGeneration) :: t1 = thirdGeneration (isSet=.true., name = 't1', &
                                                    id = 1)

    type (thirdGeneration) :: t2 = thirdGeneration (name='t2', isSet = .true., &
                                                    id = 2)

    type (thirdGeneration) :: t3 = thirdGeneration ( &
            id = 3, isSet = .true., name ='t3'     )


    type (child) :: c1 = child (4, name = 'c1')
    type (child) :: c2 = child (name = 'c2', id = 5)

    ! validate all data
    if (.not. isChildCorrect (t1%child, 1, 1.0, 't1')) error stop 1_4
    if (.not. t1%isSet) error stop 2_4

    if (.not. isChildCorrect (t2%child, 2, 1.0, 't2')) error stop 3_4
    if (.not. t2%isSet) error stop 4_4

    call addValue (t3%child, 2.0)

    if (.not. isChildCorrect (t3%child, 3, 3.0, 't3')) error stop 5_4
    if (.not. t3%isSet) error stop 6_4

    if (.not. isChildCorrect (c1_m, 20, 1.0, 'c1_m')) error stop 7_4

    if (.not. isChildCorrect (t1_m%child, 10, 1.0, 't1_m')) error stop 8_4
    if (.not. t1_m%isSet) error stop 9_4

    if (.not. isChildCorrect (c1, 4, 1.0, 'c1')) error stop 10_4
    if (.not. isChildCorrect (c2, 5, 1.0, 'c2')) error stop 11_4

    call addValue (c2, 1.0)

    if (.not. isChildCorrect (c2, 5, (1.0+1.0), 'c2')) error stop 12_4
end

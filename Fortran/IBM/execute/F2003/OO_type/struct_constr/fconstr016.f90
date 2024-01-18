! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 14, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (parent type renamed
!*                               out of the module)
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
        real*4, private :: value = 1.0
    end type

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ((b%id == intVal) .and. (b%value == realVal))
    end function
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child
        character(20) :: name
    end type

    type (newBase), save :: b1_m = newBase (1)
    type (child), save :: c1_m = child (2, name = 'c1_m')
    type (child), save :: c2_m

    contains

    subroutine initializeC2_m
        c2_m = child (newbase = b1_m, name = 'c2_m')
    end subroutine
end module


program fconstr016
use m, anotherBase => base

    type, extends(anotherBase) :: secondChild
        logical*2 :: isSet
    end type

    type (anotherBase) :: a1 = anotherBase(4)
    type (secondChild) :: s1, s3
    type (secondChild) :: s2 = secondChild (id = 5, isSet = .true.)

    s1 = secondChild (anotherbase = a1, isSet = .true.)
    s3 = secondChild (anotherbase = anotherBase (6), isSet = .true.)

    ! validate all the data
    if (.not. isBaseCorrect (a1, 4, 1.0)) error stop 1_4

    if (.not. isBaseCorrect (s1%anotherbase, 4, 1.0)) error stop 2_4
    if (.not. s1%isSet) error stop 3_4

    if (.not. isBaseCorrect (s2%anotherbase, 5, 1.0)) error stop 4_4
    if (.not. s2%isSet) error stop 5_4

    if (.not. isBaseCorrect (s3%anotherbase, 6, 1.0)) error stop 6_4
    if (.not. s3%isSet) error stop 7_4

    call validateM1Data()
end

subroutine validateM1Data()
use m1

    call initializeC2_m()

    if (.not. isBaseCorrect (b1_m, 1, 1.0)) error stop 20_4

    if (.not. isBaseCorrect (c1_m%newbase, 2, 1.0)) error stop 21_4
    if (c1_m%name /= 'c1_m') error stop 22_4

    if (.not. isBaseCorrect (c2_m%newbase, 1, 1.0)) error stop 23_4
    if (c2_m%name /= 'c2_m') error stop 24_4

end subroutine

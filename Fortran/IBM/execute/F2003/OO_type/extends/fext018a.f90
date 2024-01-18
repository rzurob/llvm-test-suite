!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext018a.f
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
!*                               via use association. child type renamed again
!*                               in third generation)
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
    end type

    type (base) :: b0_m
end module

module m1
use m, newBase => base

    type, extends(newBase) :: child
        character(20) :: name
    end type

    type (child) :: c1_m
    type (newBase) :: b1_m

    contains

    logical function isBaseCorrect (b, intVal)
        type (newBase), intent (in) :: b
        integer*4, intent(in) :: intVal

        isBaseCorrect = (b%id .eq. intVal)
    end function
end module

module m2
use m1, newChild => child
    type, extends(newChild) :: thirdGeneration
        logical*2 :: isSet = .false.
    end type

    type (thirdGeneration), save :: t1_m
    type (newChild) :: c2_m

    contains

    logical function isChildCorrect (c, intVal, charVal)
        type (newChild), intent(in) :: c
        integer*4, intent(in) :: intVal
        character(*), intent(in) :: charVal

        isChildCorrect = (isBaseCorrect (c%newbase, intVal) .and. &
                        (c%name == charVal))
    end function
end module

program fext018a
    use m2

    type (newBase) :: b1
    type (newChild) :: c1
    type (thirdGeneration) :: t1

    ! set up all the data
    b0_m%id = 0
    b1_m%id = 1
    b1%id = 2

    c1_m%id = 3
    c1_m%name = 'c1_m'

    c2_m%id = 4
    c2_m%name = 'c2_m'

    c1%id = 5
    c1%name = 'c1'

    t1_m%id = 6
    t1_m%name = 't1_m'
    t1_m%isSet = .true.

    if (t1%isSet) error stop 1_4

    t1%id = 7
    t1%name = 't1'
    t1%isSet = ('name' == 'name')

    ! validate all the data, via module functions
    if (.not. isBaseCorrect (b0_m, 0)) error stop 2_4
    if (.not. isBaseCorrect (b1_m, 1)) error stop 3_4
    if (.not. isBaseCorrect (b1, 2)) error stop 4_4
    if (.not. isBaseCorrect (c1_m%newbase, 3)) error stop 5_4
    if (.not. isBaseCorrect (c2_m%newbase, 4)) error stop 6_4
    if (.not. isBaseCorrect (c1%newbase, 5)) error stop 7_4
    if (.not. isBaseCorrect (t1_m%newbase, 6)) error stop 8_4
    if (.not. isBaseCorrect (t1%newchild%newbase, 7)) error stop 9_4

    if (.not. isChildCorrect (c1_m, 3, 'c1_m')) error stop 10_4
    if (.not. isChildCorrect (c2_m, 4, 'c2_m')) error stop 11_4
    if (.not. isChildCorrect (c1, 5, 'c1')) error stop 12_4
    if (.not. isChildCorrect (t1_m%newchild, 6, 't1_m')) error stop 13_4
    if (.not. isChildCorrect (t1%newchild, 7, 't1')) error stop 14_4

    if (.not. t1_m%isSet) error stop 15_4
    if (.not. t1%isSet) error stop 16_4
end

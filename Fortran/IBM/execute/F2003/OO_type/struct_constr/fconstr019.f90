!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr019.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (components fully default
!*                                  initialized)
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
        integer*4 ::id = 0
        real*4, private :: value = 1.0
    end type


    type, extends(base) :: child
        character(20) :: name = ''
    end type

    type, extends(child) :: thirdGeneration
        logical *1 :: isSet = .false.
    end type

    type (base), save :: b1_m = base ()
    type (child), save :: c1_m = child (1)
    type (child), save :: c2_m = child (name = 'c2_m', id = 2)
    type (child), save :: c3_m = child (base = base(), name = 'c3_m')

    type (thirdGeneration), save :: t1_m = thirdGeneration()
    type (thirdGeneration), save :: t2_m = thirdGeneration(id = 3, isSet=.true.)
    type (thirdGeneration), save :: t3_m = thirdGeneration(child = child ())

    contains

    logical function validateChildData (c, intVal, charVal)
        type (child), intent(in) :: c
        integer*4, intent(in) :: intVal
        character(*), intent(in) :: charVal

        validateChildData = ((c%id == intVal) .and. (c%name == charVal))
    end function
end module


program fconstr019
use m
    type (thirdGeneration) :: t1 = thirdGeneration(name = 't1')
    type (thirdGeneration) :: t2 = thirdGeneration(id = 1, name = 't2')
    type (thirdGeneration) :: t3 = thirdGeneration(id = 2, name = 't3', isSet = .true.)

    type (child) :: c1 = child()
    type (child) :: c2 = child (name = 'c2')
    type (child) :: c3 = child (base = base(10))


    ! validate all data
    if (.not. validateChildData(c1, 0, '')) error stop 1_4

    if (.not. validateChildData(c2, 0, 'c2')) error stop 2_4

    if (.not. validateChildData(c3, 10, '')) error stop 3_4

    if ((.not. validateChildData(t1%child, 0, 't1')) .or. t1%isSet) error stop 4_4

    if ((.not. validateChildData(t2%child, 1, 't2')) .or. t2%isSet) error stop 5_4

    if ((.not. validateChildData(t3%child,2,'t3')) .or. (.not. t3%isSet)) &
            error stop 6_4


    if (b1_m%id /= 0) error stop 7_4

    if (.not. validateChildData(c1_m, 1, '')) error stop 8_4

    if (.not. validateChildData(c2_m, 2, 'c2_m')) error stop 9_4

    if (.not. validateChildData(c3_m, 0, 'c3_m')) error stop 10_4

    if ((.not. validateChildData(t1_m%child, 0, '')) .or. &
        t1_m%isSet) error stop 11_4

    if ((.not. validateChildData(t2_m%child, 3, '')) .or. &
        (.not. t2_m%isSet)) error stop 12_4

    if ((.not. validateChildData(t3_m%child, 0, '')) .or. &
        t3_m%isSet) error stop 13_4



    ! re-assign t2 and t3 using struct_constructors
    t2 = thirdGeneration (child = child(base = base()))
    t3 = thirdGeneration (base = base())

    if ((.not. validateChildData(t2%child,0,'')) .or. t2%isSet) error stop 14_4

    if ((.not. validateChildData(t3%child,0,'')) .or. t3%isSet) error stop 15_4
end

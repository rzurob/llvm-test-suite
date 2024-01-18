! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/23/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (derived type with
!*                               inherited type-bound proc)
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
        contains

        procedure, nopass :: id => baseID
    end type

    type, extends(base) :: child
        character*20 :: name = ''
    end type

    type, extends(child) :: thirdGeneration
    end type

    type (base) :: b1_m = base ()
    type (child), save :: c1_m = child (name = 'c1_m')
    type (thirdGeneration), save :: t1_m = thirdGeneration ('t1_m')
    contains

    subroutine baseID
        print *, 'base'
    end subroutine
end module

program fconstr028
use m

    type (thirdGeneration) :: t1 = thirdGeneration(name = 't1')
    type (thirdGeneration) :: t2 = thirdGeneration('t2')
    type (thirdGeneration) :: t3 = thirdGeneration()

    if (t1%name /= 't1') error stop 1_4

    if (t1%name /= t1%child%name) error stop 2_4

    if (t2%name /= 't2') error stop 3_4

    if (t2%name /= t2%child%name) error stop 4_4

    if (t3%name /= '') error stop 5_4

    if (t3%name /= t3%child%name) error stop 6_4

    if (c1_m%name /= 'c1_m') error stop 7_4

    if ((t1_m%name /= 't1_m') .or. (t1_m%name /= t1_m%child%name)) error stop 8_4

    call t3%id()
end

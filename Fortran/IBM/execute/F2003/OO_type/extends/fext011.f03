! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (component inherited,
!*                               parent's components accessed via short-hand or
!*                               full name in a third generation, derived
!*                               types defined in two modules)
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

    type, extends(base) :: child
        character(20) :: name
    end type
end module

module m1
use m
    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type (thirdGeneration) :: t1_m
    type (child) :: c1_m

end module

program fext011
    use m1

    type (thirdGeneration) :: t1
    type (child) :: c1

    t1%child%base%id = 100
    if (t1%id /= 100) error stop 1_4

    t1%child%id = 10
    if (t1%id /= 10) error stop 2_4

    t1%id = 1
    if (t1%id /= 1) error stop 3_4

    t1%child%name = 'Test child 1'
    if (t1%name /= 'Test child 1') error stop 4_4

    t1%name = 'Test child again'
    if (t1%name /= 'Test child again') error stop 5_4

    t1%isSet = .true.
    if (.not. t1%isSet) error stop 6_4

    c1%id = 2
    c1%name = 'c1'

    t1_m%child%id = 3
    t1_m%name = 't1_m'
    t1_m%isSet = (abs(-10) > 2)

    c1_m%id = 4
    c1_m%name = 'c1_m'

    if ((c1%id /= 2) .or. (c1%name /= 'c1')) error stop 7_4
    if (c1%base%id /= 2) error stop 8_4

    if ((t1_m%id /= 3) .or. (t1_m%name /= 't1_m') .or. .not. t1_m%isSet) &
                    error stop 9_4

    if ((t1_m%id /= t1_m%child%id) .or. (t1_m%id /= t1_m%base%id) .or. &
        (t1_m%id /= t1_m%child%base%id)) error stop 10_4

    if (t1_m%name /= t1_m%child%name) error stop 11_4

    if ((c1_m%id /= 4) .or. (c1_m%name /= 'c1_m')) error stop 12_4

    if (c1_m%base%id /= 4) error stop 13_4
end
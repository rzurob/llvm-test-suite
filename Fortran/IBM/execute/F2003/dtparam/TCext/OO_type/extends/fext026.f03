! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : an extended type introduced via use and only
!*                               the extended type is exposed
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
    type base(ki,kv)
        integer, kind :: ki,kv
        integer(ki) :: id
        real(kv), private :: value
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    contains

    subroutine setValue (b, realVal)
        type (base(4,4)), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base(4,4)), intent(in) :: b

        getValue = b%value
    end function

end module

module m1
    use m, only: child, setValue, getValue

    type, extends (child) :: thirdGeneration(kl)
        integer, kind :: kl
        logical(kl) isSet
    end type

    type (child(4,4,20)) :: c1_m
    type (thirdGeneration(4,4,20,2)) :: t1_m
end module

program fext026
    use m1

    type (child(4,4,20)) :: c1
    type (thirdGeneration(4,4,20,2)) :: t1

    call setValue (c1%base, 10.0)
    c1%id = 100
    c1%name = 'data c1'

    if (c1%base%id /= 100) error stop 1_4
    if (c1%name /= 'data c1') error stop 2_4
    if (getValue(c1%base) /= 10.0) error stop 3_4

    t1%child%base%id = 10
    t1%name = 'data t1'
    t1%isSet = .true.
    call setValue (t1%child%base, -1.0)

    if (t1%id /= 10) error stop 4_4
    if (t1%name /= 'data t1') error stop 5_4
    if (.not. t1%isSet) error stop 6_4
    if (getValue(t1%child%base) /= -1.0) error stop 7_4

    call setValue (c1%base, 1000.0)

    call setValue (t1_m%base, 2000.0)

    t1_m%child%id = 500
    c1_m%base%id = 300

    c1_m%name = 'c1_m'
    t1_m%name = 't1_m'

    t1_m%isSet = (getValue(t1_m%child%base) == 2000.0)

    if (t1_m%id /= 500) error stop 8_4
    if (c1_m%id /= 300) error stop 9_4
    if (t1_m%name /= 't1_m') error stop 10_4
    if (c1_m%name /= 'c1_m') error stop 11_4

    if (.not. t1_m%isSet) error stop 12_4

    if (c1_m%id /= c1_m%base%id) error stop 13_4

    if ((t1_m%id /= t1_m%base%id) .or. (t1_m%id /= t1_m%child%id) .or. &
        (t1_m%id /= t1_m%child%base%id)) error stop 14_4
end

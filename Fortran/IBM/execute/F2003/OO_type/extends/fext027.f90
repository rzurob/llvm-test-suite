! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 09, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : an extended type introduced via use and only
!*                               the extended type is exposed, and base name is
!*                               reused
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
        type (base), intent(inout) :: b
        real*4, intent(in) :: realVal

        b%value = realVal
    end subroutine

    real*4 function getValue (b)
        type (base), intent(in) :: b

        getValue = b%value
    end function

end module

module m1
    use m, only: child, setValue, getValue

    type, extends (child) :: base
        logical*2 isSet
    end type

end module

program fext027
    use m1

    type (child) :: c1
    type (base) :: b1

    call setValue (c1%base, 10.0)
    c1%id = 100
    c1%name = 'data c1'

    if (c1%base%id /= 100) error stop 1_4
    if (c1%name /= 'data c1') error stop 2_4
    if (getValue(c1%base) /= 10.0) error stop 3_4

    b1%child%base%id = 10
    b1%name = 'data t1'
    b1%isSet = .true.
    call setValue (b1%child%base, -1.0)

    if (b1%id /= 10) error stop 4_4
    if (b1%name /= 'data t1') error stop 5_4
    if (.not. b1%isSet) error stop 6_4
    if (getValue(b1%child%base) /= -1.0) error stop 7_4

    ! short hand names
    if (c1%id /= c1%base%id) error stop 8_4

    if ((b1%id /= b1%child%id) .or. (b1%id /= b1%base%id) .or. &
        (b1%id /= b1%child%base%id))  error stop 9_4
end

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/10/2004
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Use defined assignment to test the
!*    dynamic types.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program definedOperation003
use m
    interface assignment(=)
        subroutine any2any(a, b)
        use m
            class(AbstractParent), intent(out) :: a
            class(AbstractParent), intent(in) :: b
        end subroutine
    end interface

    type(Base) :: b1 = Base(1)
    class(AbstractParent), allocatable :: b2
    class(Base), pointer :: b3
    type(Child) :: c1 = Child(2, "abc")

    allocate(b2, SOURCE=Base(3))
    allocate(b3, SOURCE=Child(4,"def"))

    if(.NOT. same_type_as(b1, Base(1))) error stop 1_4
    if(.NOT. same_type_as(b2, Base(1))) error stop 2_4
    if(.NOT. same_type_as(b3, Child(1,"a"))) error stop 3_4
    if(.NOT. same_type_as(c1, Child(1,"a"))) error stop 4_4

    b1 = c1
    b3 = b2
    b2 = b1
    c1 = b3

    if(.NOT. same_type_as(b1, Base(1))) error stop 5_4
    if(.NOT. same_type_as(b2, Base(1))) error stop 6_4
    if(.NOT. same_type_as(b3, Child(1,"a"))) error stop 7_4
    if(.NOT. same_type_as(c1, Child(1,"a"))) error stop 8_4

    if(b1%i /= 2) error stop 9_4

    select type(nameB2=>b2)
        type is (Base)
            if(nameB2%i /= 2) error stop 10_4
        class default
            error stop 11_4
    end select

    select type(nameB3=>b3)
        type is (Child)
            if(nameB3%i /= 3 .OR. nameB3%c /= "c=b") error stop 12_4
        class default
            error stop 13_4
    end select

    if(c1%i /= 3 .OR. c1%c /= "c=b") error stop 14_4
end

subroutine any2any(a, b)
use m
    class(AbstractParent), intent(out) :: a
    class(AbstractParent), intent(in) :: b
    select type (nameA=>a)
        type is (Child)
            select type (nameB=>b)
                type is (Child)
                    nameA%i = nameB%i
                    nameA%c = nameB%c
                type is (Base)
                    nameA%i = nameB%i
                    nameA%c = "c=b"
                class default
                    nameA%i = 888
                    nameA%c = "c=b def"
            end select
        type is (Base)
            select type (nameB=>b)
                type is (Child)
                    nameA%i = nameB%i
                type is (Base)
                    nameA%i = nameB%i
                class default
                    nameA%i = 999
            end select
        class default
            error stop 15_4
    end select
end subroutine

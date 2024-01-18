!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface. Associate the
!                              procedure pointer to a function. Poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit type(Base) (p)

    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
        procedure(func2), pointer, nopass :: pp1 => null()
    end type

    contains

    function func1(b)
        class(Base), pointer :: b
        procedure(func2), pointer :: func1

        select type (b)
            type is (Base)
                func1 => func2
            type is (Child)
                func1 => func3
            class default
                error stop 5_4
        end select
    end function

    function func2(b)
        class(Base), pointer :: b
        type(Base) :: func2

        select type (b)
            type is (Base)
                func2 = Base(-b%i)
            class default
                error stop 6_4
        end select
    end function

    function func3(b)
        class(Base), pointer :: b
        type(Base) :: func3

        select type (b)
            type is (Child)
                func3 = Base(b%i+b%j)
            class default
                error stop 7_4
        end select
    end function
end module

program functionReturn003
use m
    type(Child) :: c1

    class(Base), pointer :: b1

    if(associated(c1%pp1)) error stop 1_4

    allocate(b1, SOURCE=Base(5))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
  
    print *, "func2", c1%pp1(b1) 

    c1%pp1 => null()
    if(associated(c1%pp1)) error stop 3_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(7, 8, null()))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 4_4

    print *, "func3", c1%pp1(b1)

    deallocate(b1)

end

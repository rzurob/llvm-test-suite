!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component for
!                              sequence type. Specify proc-interface
!                              using declaration type specification.
!                              Associate the procedure pointer component
!                              with a dummy procedure pointer. The
!                              associated function is a module function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        sequence
        integer i
        integer j
    end type

    interface
        integer function interfaceFunc1(b)
        import Base
            type(Base), intent(in) :: b
        end function

        type(Base) function interfaceFunc2(b, p)
        import Base, interfaceFunc1
            type(Base), intent(in) :: b
            procedure(interfaceFunc1), pointer, intent(in) :: p
        end function
    end interface

    type Child
        sequence
        integer i
        procedure(interfaceFunc1), nopass, pointer :: pp1
        integer j
        procedure(interfaceFunc2), nopass, pointer :: pp2
    end type

    contains

    integer function func1(b)
        type(Base), intent(in) :: b
        func1 = b%i + b%j
    end function

    type(Base) function func2(b, p)
        type(Base), intent(in) :: b
        procedure(integer), pointer, intent(in) :: p
        func2 = Base(p(b),p(b)*2)
    end function
end module

program sequenceType003
use m
    type(Child) :: b1
    b1%pp1 => func1
    b1%pp2 => func2

    associate(name1=>b1%pp2(Base(10,20), b1%pp1))
        print *, "func2", name1%i, name1%j
    end associate
end

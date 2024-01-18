!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is an external
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    interface
        subroutine sub1(b)
            import Base
            class(Base), intent(in) :: b
        end subroutine

        function func1(b)
            import Base
            class(Base) :: b
            class(Base), pointer :: func1(:)
        end function
    end interface

    type, extends(Base) :: Child
        integer j
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName001a
use m

    type(Child) :: c1
    class(Base), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b%Base, b%j
        class default
            error stop 1_4
    end select

    deallocate(b1)

    allocate(b1, SOURCE=Child(20,22,null(),null()))

    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        type is (Child)
            print *, "func1 Child", b%Base, b%j
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m
    class(Base), intent(in) :: b
    select type (b)
        type is (Base)
            print *, "sub1 Base", b
        type is (Child)
            print *, "sub1 Child", b%Base, b%j
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m
    class(Base) :: b
    class(Base), pointer :: func1(:)
    select type (b)
        type is (Base)
            allocate(func1(5), SOURCE=(/(Base(j+b%i),j=1,5)/))
        type is (Child)
            allocate(func1(8), SOURCE=(/(Child(j+b%i,j+b%j, null(), null()),j=1,8)/))
        class default
            error stop 4_4
    end select
end function

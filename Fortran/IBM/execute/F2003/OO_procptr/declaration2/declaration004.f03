!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure pointer is a dummy argument
!                              and has the OPTIONAL attribute. The
!                              containing procedure is a module
!                              function. Poly and unlimited poly.
!                              Dummy arguments are pointer or
!                              allocatable.
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

    type, extends(Base) :: Child
        integer j
    end type

    contains

    integer function func1(b, c, p)
        class(Base), pointer :: b
        class(*), allocatable :: c
        procedure(func2), pointer, optional, intent(in) :: p
        if(present(p)) then
            if(associated(p)) then
                func1 = p(b, c)
            else
                func1 = 10
            endif
        else
            func1 = 10
        endif
    end function

    integer function func2(b, c)
        class(Base), pointer :: b
        class(*), allocatable :: c
        select type(b)
            type is (Base)
                select type(c)
                    type is (Base)
                        func2 = b%i + c%i
                    type is (Child)
                        func2 = b%i + c%i + c%j
                    class default
                        error stop 1_4
                end select
            type is (Child)
                select type(c)
                    type is (Base)
                        func2 = b%i + b%j + c%i
                    type is (Child)
                        func2 = b%i + b%j + c%i + c%j
                    class default
                        error stop 2_4
                end select
            class default
                error stop 3_4
        end select
    end function
end module

program declaration004
use m
    class(Base), pointer :: b1
    class(*), allocatable :: c1
    procedure(func2), pointer :: p

    p => func2

    allocate(b1, SOURCE=Base(100))
    allocate(c1, SOURCE=Child(-10, -20))
    print *, func1(b1, c1)
    print *, func1(b1, c1, p)

    deallocate(b1, c1)
    allocate(b1, SOURCE=Child(101, 102))
    allocate(c1, SOURCE=Base(-11))
    print *, func1(b1, c1)
    print *, func1(b1, c1, p)
end

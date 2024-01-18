!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The procedure pointer is a dummy argument
!                              and has the OPTIONAL attribute. The
!                              containing procedure is a module
!                              function. Poly and unlimited poly.
!                              Dummy arguments are non-pointer and
!                              non-allocatable.
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

    type Container
        procedure(func2), pointer, nopass :: p
    end type

    contains

    integer function func1(b, p)
        class(Base) :: b
        procedure(func2), pointer, optional, intent(in) :: p
        if(present(p)) then
            if(associated(p)) then
                func1 = p(b)
            else
                func1 = 10
            endif
        else
            func1 = 10
        endif
    end function

    integer function func2(b)
        class(*) :: b
        select type(b)
            type is (Base)
                func2 = b%i
            type is (Child)
                func2 = b%i + b%j
            class default
                error stop 1_4
        end select
    end function
end module

program declaration003
use m
    implicit type(Container) (c)

    class(Base), pointer :: b1

    c1%p => func2

    allocate(b1, SOURCE=Base(100))
    print *, func1(b1)
    print *, func1(b1, c1%p)

    deallocate(b1)
    allocate(b1, SOURCE=Child(101, 102))
    print *, func1(b1)
    print *, func1(b1, c1%p)
end

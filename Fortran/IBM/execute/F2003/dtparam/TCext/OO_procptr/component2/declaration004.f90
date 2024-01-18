! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/component2/declaration004.f
! opt variations: -ql -qreuse=none

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
        procedure(func2), pointer, nopass :: p
    end type

    contains

    integer function func1(b, c, p)
        class(Base(4)), pointer :: b
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
        class(Base(4)), pointer :: b
        class(*), allocatable :: c
        select type(b)
            type is (Base(4))
                select type(c)
                    type is (Base(4))
                        func2 = b%i + c%i
                    type is (Child(4))
                        func2 = b%i + c%i + c%j
                    class default
                        error stop 1_4
                end select
            type is (Child(4))
                select type(c)
                    type is (Base(4))
                        func2 = b%i + b%j + c%i
                    type is (Child(4))
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
    class(Child(4)), allocatable :: cc
    class(Base(4)), pointer :: b1
    class(*), allocatable :: c1

    allocate(Child(4)::cc)
    cc%p => func2

    allocate(b1, SOURCE=Base(4)(100))
    allocate(c1, SOURCE=Child(4)(-10, -20, null()))
    print *, func1(b1, c1)
    print *, func1(b1, c1, cc%p)

    deallocate(b1, c1)
    allocate(b1, SOURCE=Child(4)(101, 102, null()))
    allocate(c1, SOURCE=Base(4)(-11))
    print *, func1(b1, c1)
    print *, func1(b1, c1, cc%p)
end

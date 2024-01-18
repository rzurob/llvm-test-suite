! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/functionReturn001c.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!                              Unlimited poly. Dummy arguments are
!                              pointer.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    subroutine sub1(b)
        class(*), pointer, intent(in) :: b
        select type (b)
            type is (Base(4))
                print *, "sub1 Base", b
            type is (Child(4))
                print *, "sub1 Child", b
            class default
                error stop 2_4
        end select
    end subroutine

    subroutine sub2(b)
        class(*), pointer, intent(in) :: b
        select type (b)
            type is (Base(4))
                print *, "sub2 Base", b
            type is (Child(4))
                print *, "sub2 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(*), pointer, intent(in) :: b
        procedure(sub1), pointer :: func1

        select type (b)
            type is (Base(4))
                func1 => sub1
            type is (Child(4))
                func1 => sub2
            class default
                error stop 4_4
        end select
    end function
end module

module m2
use m1
    type GrandChild(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), pointer, nopass :: pp1
    end type
end module

program functionReturn001c
use m2
    class(*), pointer :: b1
    class(*), allocatable :: g1

    allocate(GrandChild(4)::g1)

    select type (g1)
        type is (GrandChild(4))
            allocate(b1, SOURCE=Base(4)(10))
            g1%pp1 => func1(b1)
            call g1%pp1(b1)

            deallocate(b1)
            allocate(b1, SOURCE=Child(4)(20, 30))
            g1%pp1 => func1(b1)
            call g1%pp1(b1)
        class default
            error stop 1_4
    end select
end

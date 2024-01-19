! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/declaration2/functionReturn001e.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!                              Poly. Dummy arguments are array.
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
    end type

    contains

    subroutine sub1(b)
        class(Base(4)), pointer, intent(in) :: b(:,:)
        select type (b)
            type is (Base(4))
                print *, "sub1 Base", b
                print *, "sub1 Base", shape(b)
            type is (Child(4))
                print *, "sub1 Child", b
                print *, "sub1 Child", shape(b)
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine sub2(b)
        class(Base(4)), pointer, intent(in) :: b(:,:)
        select type (b)
            type is (Base(4))
                print *, "sub2 Base", b
                print *, "sub2 Base", shape(b)
            type is (Child(4))
                print *, "sub2 Child", b
                print *, "sub2 Child", shape(b)
            class default
                error stop 2_4
        end select
    end subroutine

    function func1(b)
        class(Base(4)), pointer, intent(in) :: b(:,:)
        procedure(sub1), pointer :: func1

        select type (b)
            type is (Base(4))
                func1 => sub1
            type is (Child(4))
                func1 => sub2
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn001e
use m
    class(Base(4)), pointer :: b1(:,:)
    procedure(sub1), pointer :: pp1

    allocate(b1(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/),(/3,4/)))
    pp1 => func1(b1)
    call pp1(b1)

    deallocate(b1)
    allocate(b1(5,3), SOURCE=reshape((/(Child(4)(i,-i),i=1,15)/),(/5,3/)))
    pp1 => func1(b1)
    call pp1(b1)
end

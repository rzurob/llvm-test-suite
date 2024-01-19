! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/declaration2/interfaceName002h.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Poly. Intrinsic or
!                              derived type, scalar or array.
!
!                              This test case test the association
!                              status of procedure pointer with the help
!                              of ASSOCIATED(), NULL(), and NULLIFY().
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type

    contains

    subroutine sub1(b)
        class(AbstractParent(4,*)), intent(in) :: b
        select type (b)
            type is (Base(4,*))
                print *, "sub1 Base", b
            class default
                error stop 11_4
        end select
    end subroutine

    function func1(b)
        class(AbstractParent(4,*)) :: b
        class(AbstractParent(4,:)), pointer :: func1
        select type (b)
            type is (Base(4,*))
                allocate(func1, SOURCE=Base(4,20)(b%i*2))
            class default
                error stop 12_4
        end select
    end function

    subroutine sub2(b)
        class(AbstractParent(4,*)), intent(in) :: b
        select type (b)
            type is (Child(4,*))
                print *, "sub2 Child", b
            class default
                error stop 13_4
        end select
    end subroutine

    function func2(b)
        class(AbstractParent(4,*)) :: b
        class(AbstractParent(4,:)), pointer :: func2
        select type (b)
            type is (Child(4,*))
                allocate(func2, SOURCE=Child(4,20)(b%j,b%i))
            class default
                error stop 14_4
        end select
    end function
end module

program interfaceName002h
use m
    procedure(sub1), pointer :: pp1 => null()
    procedure(func1), pointer :: pp2 => null()
    class(AbstractParent(4,:)), pointer :: b1

    if(associated(pp1)) error stop 1_4
    if(associated(pp2)) error stop 2_4

    pp1 => sub1
    pp2 => func1

    if(.NOT. associated(pp1)) error stop 3_4
    if(.NOT. associated(pp2)) error stop 4_4

    allocate(b1, SOURCE=Base(4,20)(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(4,*))
            print *, "func1 Base", b
        class default
            error stop 5_4
    end select

    nullify(pp1)
    pp2 => null()

    if(associated(pp1)) error stop 6_4
    if(associated(pp2)) error stop 7_4

    pp1 => sub2
    pp2 => func2

    if(.NOT. associated(pp1)) error stop 8_4
    if(.NOT. associated(pp2)) error stop 9_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Child(4,*))
            print *, "func2 Child", b
        class default
            error stop 10_4
    end select
end

! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/component2/interfaceName001e.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is an external
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              dummy arguments are pointer. Use abstract
!                              type. And interface-name is different
!                              from the name of associated procedure.
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

    interface
        subroutine interfaceSub1(b)
        import AbstractParent
            class(AbstractParent(4,*)), pointer, intent(in) :: b
        end subroutine

        function interfaceFunc1(b)
        import AbstractParent
            class(AbstractParent(4,*)), pointer, intent(in) :: b
            class(AbstractParent(4,:)), pointer :: interfaceFunc1
        end function
    end interface

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
        procedure(interfaceSub1), pointer, nopass :: pp1
        procedure(interfaceFunc1), pointer, nopass :: pp2
    end type
end module

program interfaceName001e
use m
    interface
        subroutine sub1(b)
        import AbstractParent
            class(AbstractParent(4,*)), pointer, intent(in) :: b
        end subroutine

        function func1(b)
        import AbstractParent
            class(AbstractParent(4,*)), pointer, intent(in) :: b
            class(AbstractParent(4,:)), pointer :: func1
        end function
    end interface

    class(AbstractParent(4,20)), pointer :: b1
    class(AbstractParent(4,20)), pointer :: c1

    allocate(c1, SOURCE=Child(4,20,20,4,20,4)(1,1,null(),null()))

    select type (c1)
        class is (Child(4,*,*,4,*,4))
            c1%pp1 => sub1
            c1%pp2 => func1

            allocate(b1, SOURCE=Base(4,20,20,4)(10))
            call c1%pp1(b1)
            select type (b=>c1%pp2(b1))
                type is (Base(4,*,*,4))
                    print *, "func1 Base", b
                type is (Child(4,*,*,4,*,4))
                    print *, "func1 Child", b%Base, b%j 
                class default
                    error stop 1_4
            end select

            deallocate(b1)
            allocate(b1, SOURCE=Child(4,20,20,4,20,4)(20,22,null(),null()))
            call c1%pp1(b1)
            select type (b=>c1%pp2(b1))
                type is (Base(4,*,*,4))
                    print *, "func1 Base", b
                type is (Child(4,*,*,4,*,4))
                    print *, "func1 Child", b%Base, b%j 
                class default
                    error stop 2_4
            end select
        class default
            error stop 3_4
    end select
end

subroutine sub1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent(4,*)), pointer, intent(in) :: b
    select type (b)
        type is (Base(4,*,*,4))
            print *, "sub1 Base", b
        type is (Child(4,*,*,4,*,4))
            print *, "sub1 Child", b%Base, b%j 
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m, only : AbstractParent, Base, Child
    class(AbstractParent(4,*)), pointer, intent(in) :: b
    class(AbstractParent(4,:)), pointer :: func1
    select type (b)
        type is (Base(4,*,*,4))
            allocate(func1, SOURCE=Base(4,20,20,4)(b%i*2))
        type is (Child(4,*,*,4,*,4))
            allocate(func1, SOURCE=Child(4,20,20,4,20,4)(b%j,b%i, null(), null()))
        class default
            error stop 4_4
    end select
end function

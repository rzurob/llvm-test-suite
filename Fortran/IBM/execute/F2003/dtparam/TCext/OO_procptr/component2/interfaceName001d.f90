! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/interfaceName001d.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
!                              procedure. Unlimited poly, scalar or
!                              array.
!
!                              This test case use explicit interface to
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name. The dummy
!                              arguments are allocatable.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

module m2
use m1
    interface
        subroutine sub1(b)
        use m1
            class(*), allocatable, intent(in) :: b
        end subroutine

        function func1(b)
        use m1
            class(*), allocatable, intent(in) :: b
            class(Base(:,4)), pointer :: func1
        end function
    end interface

    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName001d
use m2
    type(Container(4,20)) :: c1
    class(*), allocatable :: b1

    c1 = Container(4,20)(null(), null())

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(20,4)(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(*,4))
            print *, "func1 Base", b
        type is (Child(*,4))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4)(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(*,4))
            print *, "func1 Base", b
        type is (Child(*,4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m2
    class(*), allocatable, intent(in) :: b
    select type (b)
        type is (Base(*,4))
            print *, "sub1 Base", b
        type is (Child(*,4))
            print *, "sub1 Child", b
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m2
    class(*), allocatable, intent(in) :: b
    class(Base(:,4)), allocatable :: func1
    select type (b)
        type is (Base(*,4))
            allocate(func1, SOURCE=Base(20,4)(b%i*2))
        type is (Child(*,4))
            allocate(func1, SOURCE=Child(20,4)(b%j,b%i))
        class default
            error stop 4_4
    end select
end function

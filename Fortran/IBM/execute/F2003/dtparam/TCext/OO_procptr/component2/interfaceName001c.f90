! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/component2/interfaceName001c.f
! opt variations: -ql -qreuse=none

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
!                              actual procedure associated has the same
!                              name as interface-name. The dummy
!                              arguments are pointer.
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

    interface
        subroutine sub1(b)
        import Base
            class(Base(4)), pointer, intent(in) :: b
        end subroutine

        function func1(b)
        import Base
            class(Base(4)), pointer, intent(in) :: b
            class(Base(4)), pointer :: func1
        end function
    end interface
end module

module m2
use m1
    type, extends(Base) :: Child    ! (4)
        integer(k1) j
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName001c
use m2
    implicit type(Child(4)) (c)

    class(Base(4)), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(4)(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b%Base, b%j 
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(20,22, null(), null()))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b%Base, b%j 
        class default
            error stop 2_4
    end select
end

subroutine sub1(b)
use m2
    class(Base(4)), pointer, intent(in) :: b
    select type (b)
        type is (Base(4))
            print *, "sub1 Base", b
        type is (Child(4))
            print *, "sub1 Child", b%Base, b%j 
        class default
            error stop 3_4
    end select
end subroutine

function func1(b)
use m2
    class(Base(4)), pointer, intent(in) :: b
    class(Base(4)), pointer :: func1
    select type (b)
        type is (Base(4))
            allocate(func1, SOURCE=Base(4)(b%i*2))
        type is (Child(4))
            allocate(func1, SOURCE=Child(4)(b%j,b%i, null(), null()))
        class default
            error stop 4_4
    end select
end function

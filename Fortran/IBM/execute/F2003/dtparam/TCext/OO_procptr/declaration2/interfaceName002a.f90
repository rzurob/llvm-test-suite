! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/declaration2/interfaceName002a.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type

    contains

    subroutine sub1(b)
        class(Base(*,4)), intent(in) :: b
        select type (b)
            type is (Base(*,4))
                print *, "sub1 Base", b
            type is (Child(*,4,*,4))
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(Base(*,4)) :: b
        class(Base(:,4)), pointer :: func1(:)
        select type (b)
            type is (Base(*,4))
                allocate(func1(5), SOURCE=(/(Base(20,4)(j+b%i),j=1,5)/))
            type is (Child(*,4,*,4))
                allocate(func1(8),SOURCE=(/(Child(20,4,20,4)(j+b%i,j+b%j),j=1,8)/))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002a
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    class(Base(:,4)), pointer :: b1

    pp1 => sub1
    pp2 => func1

    allocate(b1, SOURCE=Base(20,4)(10))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(*,4))
            print *, "func1 Base", b
        type is (Child(*,4,*,4))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4,20,4)(20,22))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(*,4))
            print *, "func1 Base", b
        type is (Child(*,4,*,4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end

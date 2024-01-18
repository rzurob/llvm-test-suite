! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/declaration2/functionReturn002d.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using declaration type
!                              specification. Poly, dummy arguments are
!                              allocatable and are arrays, and return
!                              is pointer.
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

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    contains

    function func1(b)
        class(Base(:,4)), allocatable, intent(in) :: b(:,:)
        !procedure(type(Base)), pointer :: func1
         procedure(func2), pointer :: func1

        select type (b)
            type is (Base(*,4))
                func1 => func2
            type is (Child(*,4))
                func1 => func3
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(Base(:,4)), allocatable, intent(in) :: b(:,:)
        type(Base(:,4)), pointer :: func2
        select type (b)
            type is (Base(*,4))
                allocate(func2, SOURCE=Base(20,4)(size(b)))
            class default
                error stop 2_4
        end select
    end function

    function func3(b)
        class(Base(:,4)), allocatable, intent(in) :: b(:,:)
        type(Base(:,4)), pointer :: func3
        select type (b)
            type is (Child(*,4))
                allocate(func3, SOURCE=Base(20,4)(size(b)*2))
            class default
                error stop 3_4
        end select
    end function
end module

program functionReturn002d
use m
    class(Base(:,4)), allocatable :: b1(:,:)
    !procedure(type(Base)), pointer :: pp1
     procedure(func2), pointer :: pp1

    allocate(b1(4,3), SOURCE=reshape((/(Base(20,4)(i),i=1,12)/),(/4,3/)))
    pp1 => func1(b1)
    print *, "func2", pp1(b1)

    deallocate(b1)
    allocate(b1(5,2), SOURCE=reshape((/(Child(20,4)(i,i+2),i=1,10)/),(/5,2/)))
    pp1 => func1(b1)
    print *, "func3", pp1(b1)
end

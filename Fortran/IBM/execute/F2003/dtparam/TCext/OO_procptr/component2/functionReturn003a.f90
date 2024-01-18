! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_procptr/component2/functionReturn003a.f
! opt variations: -qnok -qnol -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface. Associate the
!                              procedure pointer to a function.
!                              Unlimited poly.
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

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type

    contains

    function func1(b)
        class(*), allocatable :: b
       ! procedure(type(Base)), pointer :: func1
        procedure(func2), pointer :: func1

        select type (b)
            type is (Base(*,4))
                func1 => func2
            type is (Child(*,4,*,4))
                func1 => func3
            class default
                error stop 5_4
        end select
    end function

    function func2(b)
        class(*), allocatable :: b
        type(Base(20,4)) :: func2

        select type (b)
            type is (Base(*,4))
                func2 = Base(20,4)(-b%i)
            class default
                error stop 6_4
        end select
    end function

    function func3(b)
        class(*), allocatable :: b
        type(Base(20,4)) :: func3

        select type (b)
            type is (Child(*,4,*,4))
                func3 = Base(20,4)(b%i+b%j)
            class default
                error stop 7_4
        end select
    end function
end module

module m2
use m1
    implicit type(Base(20,4)) (p)

    type Container(k3,n3)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n3
        procedure(func2), pointer, nopass :: pp1 => null()
    end type
end module

program functionReturn003a
use m2
    type(Container(4,20)) :: c1

    class(*), allocatable :: b1
    if(associated(c1%pp1)) error stop 1_4

    allocate(b1, SOURCE=Base(20,4)(5))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 2_4
    print *, "func2", c1%pp1(b1)

    c1%pp1 => null()
    if(associated(c1%pp1)) error stop 3_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4,20,4)(7, 8))
    c1%pp1 => func1(b1)
    if(.NOT. associated(c1%pp1)) error stop 4_4
    print *, "func3", c1%pp1(b1)
    deallocate(b1)
end

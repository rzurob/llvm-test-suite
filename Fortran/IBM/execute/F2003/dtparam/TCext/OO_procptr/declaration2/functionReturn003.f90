! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/declaration2/functionReturn003.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface. Associate the
!                              procedure pointer to a function. Poly.
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

    function func1(b)
        class(Base(*,4)), pointer :: b
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
        class(Base(*,4)), pointer :: b
        type(Base(20,4)) :: func2

        select type (b)
            type is (Base(*,4))
                func2 = Base(20,4)(-b%i)
            class default
                error stop 6_4
        end select
    end function

    function func3(b)
        class(Base(*,4)), pointer :: b
        type(Base(20,4)) :: func3

        select type (b)
            type is (Child(*,4,*,4))
                func3 = Base(20,4)(b%i+b%j)
            class default
                error stop 7_4
        end select
    end function
end module

program functionReturn003
use m
    implicit type(Base(20,4)) (p)

    class(Base(20,4)), pointer :: b1
    procedure(func2), pointer :: pp1 => null()
    if(associated(pp1)) error stop 1_4

    allocate(b1, SOURCE=Base(20,4)(5))
    pp1 => func1(b1)
    if(.NOT. associated(pp1)) error stop 2_4
    print *, "func2", pp1(b1)

    pp1 => null()
    if(associated(pp1)) error stop 3_4

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4,20,4)(7, 8))
    pp1 => func1(b1)
    if(.NOT. associated(pp1)) error stop 4_4
    print *, "func3", pp1(b1)
end

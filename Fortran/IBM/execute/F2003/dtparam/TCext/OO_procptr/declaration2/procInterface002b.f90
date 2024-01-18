! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/declaration2/procInterface002b.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is a module function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
!
!                              Involves abstract type and implicit
!                              typing. Function returns derived type.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type

    contains

    function func1(b)
        class(AbstractParent(4,*)), intent(in) :: b
        type(Base(4,:,:,4)), allocatable :: func1
        select type (b)
            type is (Base(4,*,*,4))
                allocate(func1, SOURCE=Base(4,20,20,4)(b%i*2))
            type is (Child(4,*,*,4,*,4))
                allocate(func1, SOURCE=Base(4,20,20,4)(b%i+b%j))
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(*), allocatable, intent(in) :: b
        type(Base(4,20,20,4)) :: func2
        select type (b)
            type is (Base(4,*,*,4))
                func2 = Base(4,20,20,4)(b%i/2)
            type is (Child(4,*,*,4,*,4))
                func2 = Base(4,20,20,4)(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end module

program procInterface002b
use m
    implicit type(Base(4,20,20,4)) (b), type(Base(4,20,20,4)) (c)

    procedure(func1), pointer :: bpp1
    procedure(func2), pointer :: cpp2

    type(Base(4,20,20,4)) :: rv1
    class(AbstractParent(4,:)), pointer :: b1
    class(*), allocatable :: b2

    bpp1 => func1
    cpp2 => func2

    allocate(b1, SOURCE=Child(4,20,20,4,20,4)(4,5))
    rv1 = bpp1(b1)
    print *, "Func1", rv1

    allocate(b2, SOURCE=Base(4,20,20,4)(6))
    rv1 = cpp2(b2)
    print *, "Func2", rv1
end

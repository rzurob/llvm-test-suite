! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/procInterface002e.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is an internal
!                              function. Poly and unlimited poly.
!                              Intrinsic or derived type, scalar.
!
!                              This is a diagnostic test case.
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
    implicit type(Base(20,4)) (p)

    interface
      type(Base(20,4)) function mfunc2(b)
        import Base
        class(*), allocatable, intent(in) :: b
      end function
    end interface


    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(integer), pointer, nopass :: ipp1
        procedure(mfunc2), pointer, nopass :: pp2
    end type
end module

program procInterface002e
use m2
    implicit type(Container(4,20)) (c)

    class(Base(:,4)), pointer :: b1
    class(*), allocatable :: b2

    c1%ipp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Child(20,4)(3,4))
    print *, "func1", c1%ipp1(b1)

    allocate(b2, SOURCE=Base(20,4)(5))
    print *, "func2", c1%pp2(b2)

    contains

    integer function func1(b)
        class(Base(*,4)), intent(in) :: b
        select type (b)
            type is (Base(*,4))
                func1 = b%i
            type is (Child(*,4))
                func1 = b%i+b%j
            class default
                error stop 1_4
        end select
    end function

    type(Base(20,4)) function func2(b)
        class(*), allocatable, intent(in) :: b
        select type (b)
            type is (Base(*,4))
                func2 = Base(20,4)(-b%i)
            type is (Child(*,4))
                func2 = Base(20,4)(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end

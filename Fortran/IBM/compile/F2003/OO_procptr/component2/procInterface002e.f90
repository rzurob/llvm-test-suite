!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

module m2
use m1
    implicit type(Base) (p)

    interface
      type(Base) function mfunc2(b)
        import Base
        class(*), allocatable, intent(in) :: b
      end function
    end interface

    type Container
        procedure(integer), pointer, nopass :: ipp1
        procedure(mfunc2), pointer, nopass :: pp2
    end type
end module

program procInterface002e
use m2
    implicit type(Container) (c)

    class(Base), pointer :: b1
    class(*), allocatable :: b2

    c1%ipp1 => func1
    c1%pp2 => func2

    allocate(b1, SOURCE=Child(3,4))
    print *, "func1", c1%ipp1(b1)

    allocate(b2, SOURCE=Base(5))
    print *, "func2", c1%pp2(b2)

    contains

    integer function func1(b)
        class(Base), intent(in) :: b
        select type (b)
            type is (Base)
                func1 = b%i
            type is (Child)
                func1 = b%i+b%j
            class default
                error stop 1_4
        end select
    end function

    type(Base) function func2(b)
        class(*), allocatable, intent(in) :: b
        select type (b)
            type is (Base)
                func2 = Base(-b%i)
            type is (Child)
                func2 = Base(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end

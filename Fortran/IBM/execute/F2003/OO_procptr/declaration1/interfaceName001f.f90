!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an external subroutine or
!                              an external function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface to
!                              declare the interface-name. The actual
!                              procedure associated has the same name
!                              as interface-name.
!                              The purpose of this test case is to test
!                              the association status of procedure
!                              pointers, with the help of
!                              NULL() and NULLIFY().
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base), intent(in) :: b
        end subroutine

        integer function ifunc1()
        end function

        real function func2(b)
        import Base
            type(Base) :: b
        end function
    end interface
end module

program interfaceName001f
use m
    procedure(sub1), pointer :: pp1 => null()
    procedure(ifunc1), pointer :: pp2 => null()
    procedure(func2), pointer :: pp3 => null()

    if(associated(pp1)) error stop 1_4
    if(associated(pp2)) error stop 2_4
    if(associated(pp3)) error stop 3_4

    pp1 => sub1
    pp2 => ifunc1
    pp3 => func2

    if(.NOT. associated(pp1)) error stop 4_4
    if(.NOT. associated(pp2)) error stop 5_4
    if(.NOT. associated(pp3)) error stop 6_4

    call pp1(10, Base(11))
    print *, "ifunc1", pp2()
    print *, "func2", int(pp3(Base(5)))

    nullify(pp1)
    nullify(pp2)
    nullify(pp3)

    if(associated(pp1)) error stop 7_4
    if(associated(pp2)) error stop 8_4
    if(associated(pp3)) error stop 9_4
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function ifunc1()
    ifunc1 = 20
end function

real function func2(b)
use m, only : Base
    type(Base) :: b
    func2 = b%i
end function

! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/interfaceName001g.f
! opt variations: -qnok -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either an
!                              external subroutine or an external
!                              function. Non-poly. Intrinsic or derived
!                              type, scalar.
!
!                              This test case use explicit interface to
!                              declare the interface-name. The actual
!                              procedure associated has the same name
!                              as interface-name.
!                              The purpose of this test case is to test
!                              the association status of procedure
!                              pointer component, with the help of
!                              NULL() and NULLIFY().
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

    interface
        subroutine sub1(i, b)
        import Base
            integer, intent(in) :: i
            type(Base(*,4)), intent(in) :: b
        end subroutine

        integer function ifunc1()
        end function

        real function func2(b)
        import Base
            type(Base(*,4)) :: b
        end function
    end interface
end module

program interfaceName001g
use m
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub1), nopass, pointer :: pp1 => null()
        procedure(ifunc1), nopass, pointer :: pp2 => null()
        procedure(func2), nopass, pointer :: pp3 => null()
    end type

    type(Container(4,20)) :: b1

    if(associated(b1%pp1)) error stop 1_4
    if(associated(b1%pp2)) error stop 2_4
    if(associated(b1%pp3)) error stop 3_4

    b1%pp1 => sub1
    b1%pp2 => ifunc1
    b1%pp3 => func2

    if(.NOT. associated(b1%pp1)) error stop 4_4
    if(.NOT. associated(b1%pp2)) error stop 5_4
    if(.NOT. associated(b1%pp3)) error stop 6_4

    call b1%pp1(10, Base(20,4)(11))
    print *, "ifunc1", b1%pp2()
    print *, "func2", int(b1%pp3(Base(20,4)(5)))

    nullify(b1%pp1)
    nullify(b1%pp2)
    nullify(b1%pp3)

    if(associated(b1%pp1)) error stop 7_4
    if(associated(b1%pp2)) error stop 8_4
    if(associated(b1%pp3)) error stop 9_4
end

subroutine sub1(i, b)
use m, only : Base
    integer, intent(in) :: i
    type(Base(*,4)), intent(in) :: b
    print *, "sub1", i, b
end subroutine

integer function ifunc1()
    ifunc1 = 20
end function

real function func2(b)
use m, only : Base
    type(Base(*,4)) :: b
    func2 = b%i
end function

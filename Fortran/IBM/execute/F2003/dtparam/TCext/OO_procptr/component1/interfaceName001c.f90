! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/interfaceName001c.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare a procedure pointer component
!                              using procedure declaration statement.
!                              specify procedure interface using
!                              interface-name, which is either an
!                              external subroutine or an external
!                              function. Non-poly. Intrinsic or derived
!                              type, scalar.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              external procedures. It also involves
!                              implicit typing. The actual procedure
!                              name has the same name as interface-name.
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
        function func1(b)
        import Base
            implicit integer (b), type(Base(20,4)) (f)
        end function
    end interface

    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(func1), nopass, pointer :: pp1
    end type
end module

program interfaceName001c
use m
    implicit integer (b), type(Base(20,4)) (f), type(Container(4,20)) (c)

    c1%pp1 => func1
    b1 = 5
    print *, "func1", c1%pp1(b1)
end

function func1(b)
use m, only : Base
    implicit integer (b), type(Base(20,4)) (f)
    func1%i = b
end function

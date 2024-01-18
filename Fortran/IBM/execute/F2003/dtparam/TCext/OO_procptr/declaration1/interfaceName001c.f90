! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/interfaceName001c.f
! opt variations: -qnol

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
end module

program interfaceName001c
use m
    implicit integer (b), type(Base(20,4)) (f)

    procedure(func1), pointer :: pp1
    pp1 => func1
    b1 = 5
    print *, "func1", pp1(b1)
end

function func1(b)
use m, only : Base
    implicit integer (b), type(Base(20,4)) (f)
    func1%i = b
end function

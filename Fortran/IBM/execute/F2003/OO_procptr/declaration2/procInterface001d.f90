!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The procedure pointer
!                              is a dummy argument. The associated
!                              function is an intrinsic function.
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

    type, extends(Base) :: Child
        integer j
    end type

    contains

    type(Child) function func2(r, p)
        real, intent(in) :: r
        procedure(real), pointer, intent(in) :: p
        func2 = Child(int(p(r)),-int(p(r)))
    end function
end module

program procInterface001d
use m
    procedure(real), pointer :: pp1
    !procedure(type(Child)), pointer :: pp2
     procedure(func2), pointer :: pp2

    pp1 => sin
    pp2 => func2

    print *, pp2(3.1415926/2, pp1)
end

! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/interfaceName003a.f
! opt variations: -qnok -ql

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name. The procedure pointer
!                              is a dummy argument, and the associated
!                              procedure is given using a procedure
!                              pointer component. Non-poly. Intrinsic
!                              or derived type, scalar.
!
!                              This test case uses explicit interface
!                              and interface implied by use association
!                              to declare interface-name. The actual
!                              procedure associated has the same name as
!                              interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    contains

    integer function func1(b)
        type(Base(4)), intent(in) :: b
        func1 = b%i
    end function
end module

program interfaceName003a
use m
    interface
        subroutine sub1(i, b, p)
        use m
            integer, intent(in) :: i
            type(Base(4)), intent(in) :: b
            procedure(func1), pointer, intent(in):: p
        end subroutine
    end interface

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
    end type

    type(Container(4)) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1

    call c1%pp1(10, Base(4)(11), c1%pp2)
end

subroutine sub1(i, b, p)
use m
    integer, intent(in) :: i
    type(Base(4)), intent(in) :: b
    procedure(func1), pointer, intent(in):: p

    if(associated(p)) then
        print *, "sub1", p(b)
    else
        error stop 1_4
    end if
end subroutine

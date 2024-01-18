! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/declaration1/interfaceName002f.f
! opt variations: -ql

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
!                              which is either a module subroutine or
!                              a module function. Non-poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments of
!                              the associated procedure are arrays.
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

    subroutine sub1(i)
        integer, intent(in) :: i(2,4)
        print *, "sub1", i
    end subroutine

    integer function func1(b)
        integer :: b(3,2)
        print *, "func1", b
        func1 = size(b)
    end function

    integer function func2(b)
        type(Base(4)) :: b(5,3)
        print *, "func2", b
        func2 = size(b)
    end function
end module

program interfaceName002f
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    procedure(func2), pointer :: pp3

    integer rv
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/))

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(reshape((/(i,i=1,8)/),(/2,4/)))

    rv = pp2(i1)
    print *, "Func1", rv

    rv = pp3(reshape((/(Base(4)(i),i=11,25)/),(/5,3/)))
    print *, "Func2", rv
end

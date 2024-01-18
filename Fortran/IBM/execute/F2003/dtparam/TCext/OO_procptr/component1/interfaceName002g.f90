! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/interfaceName002g.f
! opt variations: -qnok -ql

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
!                              interface-name, which is either a
!                              module subroutine or a module function
!                              Non-poly. Intrinsic or derived type,
!                              scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare
!                              interface-name. The actual procedure
!                              associated has the same name as
!                              interface-name. The dummy arguments of
!                              the associated procedure are arrays. This
!                              test case involves sequence association
!                              where the size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
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
        integer :: b(3:*)
        print *, "func1", b(:8)
        func1 = size(b(:8))
    end function

    integer function func2(b)
        type(Base(4)) :: b(5,3)
        print *, "func2", b
        print *, "func2", shape(b)
        func2 = size(b)
    end function
end module

module m2
use m1
    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type
end module

program interfaceName002g
use m2
    type(Container(4)) :: c1
    integer rv
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/),(/-1/),(/2,1/))

    c1%pp1 => sub1
    c1%pp2 => func1
    c1%pp3 => func2

    call c1%pp1(reshape((/(i,i=1,18)/),(/3,3,3/),(/-1,-2/),(/2,3,1/)))

    rv = c1%pp2(i1)
    print *, "Func1", rv

    rv = c1%pp3(reshape((/(Base(4)(i),i=11,25)/),(/4,3,2/), &
     (/Base(4)(-1),Base(4)(-2)/), (/2,3,1/)))
    print *, "Func2", rv
end

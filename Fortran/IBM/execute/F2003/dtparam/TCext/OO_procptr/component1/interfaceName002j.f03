! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/interfaceName002j.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
!                              interface-name. The container data entity
!                              is an array. The purpose of this test
!                              case is to see the effect of invoking
!                              procedure pointer component via array.
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

    contains

    subroutine sub1(i)
        integer, intent(in) :: i
        print *, "sub1", i
    end subroutine

    integer function func1(b)
        integer :: b
        func1 = b * 2
    end function

    function func2(b)
        type(Base(*,4)) :: b
        type(Base(20,4)) :: func2
        func2 = Base(20,4)(b%i+2)
    end function
end module

module m2
use m1
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub1), nopass, pointer :: pp1
        procedure(func1), nopass, pointer :: pp2
        procedure(func2), nopass, pointer :: pp3
    end type
end module

program interfaceName002j
use m2
    type(Container(4,20)) :: c1(2,3)

    do i=1,2
        do j=1,3
            c1(i,j)%pp1 => sub1
            c1(i,j)%pp2 => func1
            c1(i,j)%pp3 => func2
        end do
    end do

    do i=1,2
        do j=1,3
            call c1(i,j)%pp1(i+j)
            print *, "func1", c1(i,j)%pp2(i+j)
            print *, "func2", c1(i,j)%pp3(Base(20,4)(i+j))
        end do
    end do
end
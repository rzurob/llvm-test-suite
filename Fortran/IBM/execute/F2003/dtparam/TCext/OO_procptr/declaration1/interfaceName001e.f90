! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=self /tstdev/OO_procptr/declaration1/interfaceName001e.f
! opt variations: -qnol -qdeferredlp -qreuse=none

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
!                              declare the interface-name before calling
!                              external subroutine and function. The
!                              actual procedure associated has the same
!                              name as interface-name. The return value
!                              of the associated function is array and
!                              is either allocatable or pointer. The
!                              dummy arguments are also arrays.
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

    type Child(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
        integer(k2)      j
    end type
end module

program interfaceName001e
use m
    interface
        subroutine sub1(b, c)
        use m
            type(Base(*,4)), intent(in) :: b(5)
            type(Child(*,4)), intent(in) :: c(2,4)
        end subroutine

        function func1(i)
            integer, intent(in) :: i(8)
            integer, allocatable :: func1(:,:)
        end function

        function func2(b)
        use m
            type(Base(*,4)) :: b(3,5)
            type(Child(20,4)), pointer :: func2(:)
        end function
    end interface

    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    procedure(func2), pointer :: pp3

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1((/(Base(20,4)(i),i=1,5)/), reshape((/(Child(20,4)(i,-i),i=11,18)/), &
     (/2,4/)))
    print *, "func1", pp2((/(i,i=101,108)/))
    print *, "func1", shape(pp2((/(i,i=101,108)/)))
    print *, "func2", pp3(reshape((/(Base(20,4)(-i),i=1,15)/), (/3,5/)))
    print *, "func2", shape(pp3(reshape((/(Base(20,4)(-i),i=1,15)/), &
     (/3,5/))))
end

subroutine sub1(b, c)
use m
    type(Base(*,4)), intent(in) :: b(5)
    type(Child(*,4)), intent(in) :: c(2,4)
    print *, "sub1", b
    print *, "sub1", shape(b)
    print *, "sub1", c
    print *, "sub1", shape(c)
end subroutine

function func1(i)
    integer, intent(in) :: i(8)
    integer, allocatable :: func1(:,:)
    allocate(func1(4,3))
    func1 = reshape(i,(/4,3/),(/-1/),(/2,1/))
end function

function func2(b)
use m
    type(Base(*,4)) :: b(3,5)
    type(Child(20,4)), pointer :: func2(:)
    allocate(func2(7))
    func2 = transfer(b,Child(20,4)(1,1),7)
end function

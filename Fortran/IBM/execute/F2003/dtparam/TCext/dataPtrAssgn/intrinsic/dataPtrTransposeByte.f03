! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrTransposeByte.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!* - data-pointer is pointer component , the selector of select type
!* - data-pointer thr the selector as arg of transpose, type byte
!* - bound is func name
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    byte :: y(10,10)
    class(*), pointer :: x(:,:)
    end type
    type :: B(k2,n2)    ! (4,20)
    integer, kind          :: k2
    integer, len           :: n2
    type(A(k2,:)), pointer :: p1(:,:)
    end type
end module

    program main
    use m

    type(B(4,20)), target :: b1
    byte, allocatable :: a1(:,:)

    integer i, j
        equivalence(i,j)

    allocate(A(4,20) :: b1%p1(1,1))

    i = 3

    b1%p1(1,1)%y = reshape((/ (i,i=1,100) /), (/10,10 /) )

    b1%p1(1,1)%x(2:, 2:) => b1%p1(1,1)%y

    if ( .not. associated(b1%p1(1,1)%x, b1%p1(1,1)%y)) error stop 11
    if ( any ( lbound(b1%p1(1,1)%x) .ne. (/2,2/))) error stop 12
    if ( any ( ubound(b1%p1(1,1)%x) .ne. (/11,11/))) error stop 15

    b1%p1(1,1)%x(i:func(4), j: func(5)) => b1%p1(1,1)%x(:,7)

    if ( .not. associated(b1%p1(1,1)%x)) error stop 21
    if ( any ( lbound(b1%p1(1,1)%x) .ne. (/3,3/))) error stop 22
    if ( any ( ubound(b1%p1(1,1)%x) .ne. (/4,5/))) error stop 25

    select type(arg => b1%p1(1,1)%x)
        type is (byte)
        print *, arg
        a1 = transpose(arg)
        class default
        stop 31
    end select

    do i = 1, 3
       print *, a1(i,:)
    end do

    contains
    function func(a)
        integer a, func
        func = a
    end function
    end program

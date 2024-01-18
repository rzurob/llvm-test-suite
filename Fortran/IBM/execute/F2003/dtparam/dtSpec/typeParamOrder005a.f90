!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/08/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: For derived type components that are of
!                               procedure pointer; test the parameter using
!                               deferred parameter values.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (n)
        integer, len :: n

        double precision :: data(n)
    end type

    abstract interface
        function gen (n)
        import
            integer, intent(in) :: n
            type(A(n)) gen
        end function
    end interface


    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id
        procedure(gen), nopass, pointer :: p1 => null()
        class(A(:)), allocatable :: data(:)
    end type

    type (base(4,:)), pointer :: b1
end module

program typeParamOrder005a
use m
    interface
        subroutine updateBase4 (b, i1, p1, a1)
        use m
            type (base(4,*)), intent(inout) :: b
            integer(4), intent(in) :: i1
            procedure(gen) p1
            class(A(*)), intent(in) :: a1(:)
        end subroutine
    end interface

    logical(4), external :: precision_r8

    procedure(gen) :: genA

    integer(4) :: i1 = 100

    class(A(:)), allocatable :: a1(:)

    allocate (base(4, 25) :: b1)
    allocate (A(10) :: a1(10))

    do i = 1, 10
        a1(i)%data = (/(i*1.0d1+j, j=0,9)/)
    end do

    call updateBase4 (b1, i1, genA, a1)

    !!verify the results
    if (b1%id /= 100) error stop 1_4

    if (.not. associated(b1%p1, genA)) error stop 2_4

!    associate (x => b1%p1(b1%n))
!        do i = 1, 25
!            if (.not. precision_r8(x%data(i), i*1.0d0)) error stop 3_4
!        end do
!    end associate

    call associate_replacer (b1%p1(b1%n))

    if (.not. allocated(b1%data)) error stop 4_4

    do i = 1, 10
        do j = 1, 10
            if (.not. precision_r8(b1%data(i)%data(j), i*1.0d1+j-1)) &
                    error stop 5_4
        end do
    end do
    contains

    subroutine associate_replacer (x)
        type(A(*)), intent(in) :: x

        do i = 1, 25
            if (.not. precision_r8(x%data(i), i*1.0d0)) error stop 3_4
        end do
    end subroutine
end

subroutine updateBase4 (b, i1, p1, a1)
use m
    type (base(4,*)), intent(inout) :: b
    integer(4), intent(in) :: i1
    procedure(gen) p1
    class(A(*)), intent(in) :: a1(:)

    b%id = i1
    b1%p1 => p1
    allocate (b1%data(size(a1)), source=a1)
end subroutine

function genA (n)
use m
    integer, intent(in) :: n
    type (A(n)) genA

    genA%data = (/(i*1.0d0,i=1,n)/)
end function

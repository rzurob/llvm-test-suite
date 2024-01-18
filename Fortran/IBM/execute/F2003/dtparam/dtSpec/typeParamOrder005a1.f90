! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: 2nd part split out of typeParamOrder005a;
!                               original test case seems too complex.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (n)
        integer, len :: n

        double precision :: data(n)
    end type

    abstract interface
        function f1 (n)
        import
            integer, intent(in) :: n

            type(A(n)) f1
        end function
    end interface

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id
!        procedure(type(A(n))), nopass, pointer :: p1 => null()
        procedure(f1), nopass, pointer :: p1 => null()
        class(A(:)), allocatable :: data(:)
    end type

end module

program typeParamOrder005a1
use m
    class (base(8,:)), allocatable :: b2

    interface
        subroutine getBaseVal8 (b, i1, a1, a2)
        use m
            type (base(8,*)), intent(in) :: b
            integer(8), intent(out) :: i1
            type(A(b%n)), intent(out) :: a1
            class(A(:)), allocatable, intent(out) :: a2(:)
        end subroutine
    end interface

    logical(4), external :: precision_r8

    integer(8) :: i1
    type(A(30)) :: a1
    procedure(f1) genA

    class(A(:)), allocatable :: a2(:)

    !! set up values for b2
    allocate (base(8,30) :: b2)
    allocate (A(100):: b2%data(10))

    b2%id = 2_8**35_2
    b2%p1 => genA

    do i = 1, 10
        b2%data(i)%data = (/(i*1.0d2+j, j=0,99)/)
    end do

    allocate(A(30) :: a2(100))

    call getBaseVal8 (b2, i1, a1, a2)

    !! verify results
    if (i1/2**29 /= 64) error stop 1_4

    do i = 1, 30
        if (.not. precision_r8(a1%data(i), (31-i)*1.0d0)) error stop 2_4
    end do

    if (.not. allocated(a2)) error stop 3_4

    if (size(a2) /= 10) error stop 4_4

    do i = 1, 10
        do j = 1, 100
            if (.not. precision_r8(a2(i)%data(j), i*1.0d2+j-1)) error stop 5_4
        end do
    end do
end

subroutine getBaseVal8 (b, i1, a1, a2)
use m
    type (base(8,*)), intent(in) :: b
    integer(8), intent(out) :: i1
    type(A(b%n)), intent(out) :: a1
    class(A(:)), allocatable, intent(out) :: a2(:)

    i1 = b%id

    if (.not. associated(b%p1)) stop 10

    a1 = b%p1(b%n)

    if (allocated(b%data)) then
        allocate (a2(size(b%data)), source=b%data)
    end if
end subroutine

function genA (n)
use m
    integer, intent(in) :: n

    type(A(n)) genA

    if (n /= 30) stop 10
    genA%data = (/(i*1.0d0, i=30,1,-1)/)
end function

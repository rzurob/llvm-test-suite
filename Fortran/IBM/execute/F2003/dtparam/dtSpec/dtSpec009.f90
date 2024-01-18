
module m
    type base (n, l)
        integer, len :: n, l

        real :: data(n)
        character(l), allocatable :: names(:)
    end type

    class(base(10, 20)), allocatable :: b1

    abstract interface
        subroutine allocateBaseScalar (b, r1, c1)
            import
            class(base(n=*,l=*)), allocatable, intent(out) :: b
            real, intent(in) :: r1(b%n)
            character(*), intent(in), optional :: c1(:)
        end subroutine

        subroutine allocateBaseArray (b, r1, c1, n)
        import
            class(base(n=*,l=*)), pointer, intent(out) :: b(:)
            real, intent(in) :: r1(b%n)
            character(*), intent(in), optional :: c1(:)
            integer, intent(in) :: n
        end subroutine
    end interface

    contains

    subroutine printBaseArray (b)
        class(base(*,*)), intent(in), pointer :: b(:)

        if (associated(b)) then
            do i = lbound(b,1), ubound(b,1)
                print *, 'element ', i

                write (*, '(8f10.2)') b(i)%data

                if (allocated(b(i)%names)) then
                    print *, b(i)%names
                end if
            end do
        end if
    end subroutine
end module

program dtSpec009
use m
    procedure(allocateBaseScalar) allocateBase1
    procedure(allocateBaseArray) allocateBase2

    real, allocatable :: r1(:)
    character(20) :: c1(3)

    class(base(30, 15)), pointer :: b2(:)

    allocate (r1(30), source=(/(i*1.0, i=1,30)/))

    c1 = (/'xlftest 01', 'xlftest 02', 'xlftest 03'/)

    allocate (b1)
    allocate (b1%names(100))

    call allocateBase1 (b1, r1)
    call printB1

    call allocateBase1 (b1, r1, c1)
    call allocateBase2 (b2, r1, c1, 10)

    !! print out b1
    call printB1
    call printBaseArray(b2)
end

subroutine printB1
use m
    if (allocated(b1)) then
        print *, 'real part:'
        write (*, '(10f8.2)') b1%data
        if (allocated(b1%names)) then
            print *, 'character part:'
            print *, b1%names
        end if
    end if
end subroutine


subroutine allocateBase1 (b, r1, c1)
use m
    class(base(n=*,l=*)), allocatable, intent(out) :: b
    real, intent(in) :: r1(b%n)
    character(*), intent(in), optional :: c1(:)

    allocate (base(*,*) :: b)

    b%data = r1

    if (present(c1)) then
        allocate (b%names(size(c1)), source=c1)
    end if
end subroutine

subroutine allocateBase2 (b, r1, c1, n)
use m
    class(base(l=*,n=*)), pointer, intent(out) :: b(:)
    real, intent(in) :: r1(b%n)
    character(*), intent(in), optional :: c1(:)
    integer, intent(in) :: n

    allocate (base(*,*) :: b(n))

    do i = 1, n
        b(i)%data = r1

        if (present(c1)) then
            allocate(b(i)%names(size(c1)), source=c1)
        end if
    end do
end subroutine

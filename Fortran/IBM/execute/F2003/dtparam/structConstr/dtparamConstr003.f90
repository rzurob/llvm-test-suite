
module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    abstract interface
        function genBase (d1)
        import
            real(8), intent(in) :: d1(:)
            type(base(8,:)), pointer :: genBase
        end function
    end interface

    type container (k)
        integer, kind :: k

        procedure(genBase), pointer, nopass :: genData => null()
    end type

end module

program dtparamConstr003
use m
    procedure (genBase) genBase8

    double precision d1(100)

    type(container(8)), allocatable :: co1(:)
    type(container(8)) co2

    type(base(8,:)), pointer :: b1

    logical(4), external :: precision_r8

    d1 = (/(i*1.2d0, i=1, 100)/)

    allocate (co1(5), source=container(8)(genBase8))

    co2 = container(8)(genBase8)

    !! verify
    do i = 1, 5
        if (.not. associated(co1(i)%genData, genBase8)) error stop 1_4
    end do

    if (.not. associated(co1(3)%genData, co2%genData)) error stop 2_4

    b1 => co1(3)%genData (d1(2::2))

    if (b1%n /= 50) error stop 3_4

    do i = 1, 50
        if (.not. precision_r8(b1%data(i), sqrt(2.0d0*i*1.2d0))) error stop 4_4
    end do

    associate (x => co2%genData(d1))
        if (x%n /= 100) error stop 5_4

        do i  = 1, 100
            if (.not. precision_r8(x%data(i), sqrt(i*1.2d0))) error stop 6_4
        end do
    end associate
end

function genBase8 (d1)
use m
    real(8), intent(in) :: d1(:)
    type(base(8,:)), pointer :: genBase8

    allocate (base(8,size(d1)) :: genBase8)

    genBase8%data = sqrt(d1)
end function

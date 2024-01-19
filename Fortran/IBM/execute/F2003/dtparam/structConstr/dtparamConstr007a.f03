
module m
    type base (k, n1, n2)
        integer(1), kind :: k
        integer, len :: n1, n2

        integer(k) :: val(n1, n2)
    end type
end module

program dtparamConstr007a
use m
    type (base(8, 23, 55)) :: b1
    type (base(2,:,:)), pointer :: b2(:)

    type (base(1, 6, 20)) :: b3

    integer i1(23, 55)

    do i = 1, 23
        i1(i,:) = (/(i*100+j, j=1, 55)/)
    end do

    b1 = base(8, 23, 55)(val=i1*2_8**30)

    allocate(base(2,3,5) :: b2(10))

    b2 = base(2,3,5) (100)

    b3 = base(1, 6, 20)(reshape((/(i, i=1, 120)/), (/6, 20/)))

    !! verify
    do j = 1, 55
        do i = 1, 23
            if (b1%val(i, j)/2**30 /= i*100+j) error stop 1_4
        end do
    end do

    do i = 1, 10
        if (any(b2(i)%val /= 100)) error stop 2_4
    end do

    k = 1

    do j = 1, 20
        do i = 1, 6
            if (b3%val(i,j) /= k) error stop 3_4

            k = k + 1
        end do
    end do
end

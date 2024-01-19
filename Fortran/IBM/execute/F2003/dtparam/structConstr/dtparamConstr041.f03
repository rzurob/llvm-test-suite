! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Polymorphic pointer to be associated with
!                               a data target.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (m,l)
        integer, len :: m, l

        character(l) :: name
        type(base(k,n)) :: comp(m)
    end type
end module

module n
use m
    type container (k)
        integer, kind :: k

        class(base(k,:)), pointer :: data(:)
    end type
end module

program dtparamConstr041
use m
use n
    type (container(4)), allocatable :: co1(:)

    type (child(4,:,:,:)), allocatable, target :: c1(:)

    logical(4), external :: precision_r4

    allocate (child(4,35, 11,20) :: c1(20))

    allocate (co1(10))

    co1(1:5) = (/(container(4)(c1(i::2)), i=1,5)/)

    co1(6:10) = (/(container(4)(c1(2*i)%comp), i=6,10)/)

    !! assign values of comp through co1(6:10) --> c1(12:20:2)%comp
    do i = 6, 10
        do j = 1, 11
            co1(i)%data(j)%data = (/(k+(j-1)*100+i*1000, k=1,35)/)
        end do
    end do

    !! assign values of data through co1(5) --> c1(5:19:2)
    do i = 5, 19, 2
        do j = 1, 35
            co1(5)%data((i-3)/2)%data(j) = i*100 + j
        end do
    end do

    !! assign values of data through co1(4) --> c1(4:20:2)
    do i = 4, 20, 2
        do j = 1, 35
            co1(4)%data((i-2)/2)%data(j) = i*100 + j
        end do
    end do

    !! assign values of name through co1(3) --> c1(3:19:2)
    select type (x => co1(3)%data)
        type is (child(4,*,*,*))
            x%name = 'assigned through co1(3)'

        class default
            error stop 1_4
    end select

    !! assign values of name through co1(2) --> c1(2:20:2)
    select type (x => co1(2)%data)
        class is (child(4,*,*,*))
            x%name = 'assigned via co1(2)'

        class default
            error stop 2_4
    end select

    !! verify assigned values
    do i = 3, 10
        do j = 1, 35
            if (.not. precision_r4(co1(1)%data(i)%data(j), &
                1.0*(100*(i*2-1)+j))) error stop 2_4
        end do
    end do

    do i = 4, 20
        do j = 1, 35
            if (.not. precision_r4(c1(i)%data(j), 1.0*(100*i+j))) error stop 3_4
        end do
    end do

    if (any (c1(3:19:2)%name /= 'assigned through co1')) error stop 4_4

    if (any (c1(2:20:2)%name /= 'assigned via co1(2)')) error stop 5_4

    do i = 12, 20, 2
        do j = 1, 11
            do k = 1, 35
                if (.not. precision_r4(c1(i)%comp(j)%data(k), &
                    (k+(j-1)*100+i*500)*1.0)) error stop 6_4
            end do
        end do
    end do
end

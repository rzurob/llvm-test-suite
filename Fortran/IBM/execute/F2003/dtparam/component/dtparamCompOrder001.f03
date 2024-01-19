! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/30/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.5: comp. order)
!                               Case: Test component order in intrinsic
!                               formatted I/O; test intrinsic read.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(k) :: id
    end type

    type, extends(base) :: child(n)
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtparamCompOrder001
use m
    class(base(4)), allocatable :: b1
    class(base(8)), pointer :: b2(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (child(4, 10) :: b1)
    allocate (child(8, 30) :: b2(0:9))

    open (1, file='dtparamCompOrder001.in')

    select type (b1)
        type is (child(4, *))
            read (1, *) b1

        class default
            error stop 1_4
    end select

    select type (b2)
        type is (child(8,*))
            read (1, *) b2(:2)

            read (1, '(7(i20,/, 10(3g25.15,/)))') b2(3:)

        class default
            error stop 2_4
    end select


    !! verify the data read in
    if (b1%id /= 10) error stop 5_4

    if (any((b2%id - (/(j, j=1, 10)/))/2**30 /= 8)) error stop 6_4

    select type (b1)
        type is (child(4,*))
            do i = 1, 10
                if (.not. precision_r4 (b1%data(i), i*2.115e0)) error stop 7_4
            end do

        class default
            error stop 10_4
    end select

    select type (b2)
        type is (child(8,*))
            do i = 0, 9
                do j = 1, 30
                    if (.not. precision_r8 (b2(i)%data(j), j*3.993d42)) &
                            error stop 8_4
                end do
            end do

        class default
            error stop 20_4
    end select
end

!! to reproduce the input data, run this following program
!
!    type A
!        integer(4) :: id
!        real(4) :: data(10)
!    end type
!
!    type B
!        integer(8) :: id
!        real(8) :: data(30)
!    end type
!
!    type (A) :: a1
!    type (B) :: b1(10)
!
!    a1 = A(10, (/(i*2.115e0, i=1, 10)/))
!
!    b1%id = 2_8**33 + (/(j, j=1, 10)/)
!
!    do i = 1, 10
!        b1(i)%data = (/(j*3.993d42, j=1, 30)/)
!    end do
!
!    print *, a1
!    print *, b1(:3)
!
!    write (*, '(7(i20,/, 10(3g25.15,/)))') b1(4:)
!    end

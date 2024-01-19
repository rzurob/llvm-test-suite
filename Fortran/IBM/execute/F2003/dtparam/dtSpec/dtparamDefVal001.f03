! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Test the default value for type
!                               parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 100

        real(k) :: data(n) = -1.0d0
    end type

    type, extends(base) :: child (l)
        integer, len :: l = 20

        integer(k) :: id = -1
        character(l) :: name = 'default'
    end type

    class(base), allocatable :: b1(:)
end module

program dtparamDefVal001
use m
    class(base), pointer :: b2

    logical(4), external :: precision_r8

    allocate (b2)
    allocate (child :: b1(10))

    select type (x => b1(::2))
        type is (child(l=*,n=*))
            x = (/(child(data=(/(j*1.0d3+i*1.0d0, i=1, 100)/), id=j,&
                    name='b1'), j=1,10,2)/)

        class default
            error stop 1_4
    end select

    !! verify results
    do i = 1, 100
        if (.not. precision_r8 (b2%data(i), -1.0d0)) error stop 2_4
    end do

    select type (b1)
        class is (child(n=*,l=*))
            do i = 1, 10
                if (mod(i, 2) == 0) then
                    if ((b1(i)%id /= -1) .or. (b1(i)%name /= 'default')) &
                        error stop 4_4

                    do j = 1, 100
                        if (.not. precision_r8 (b1(i)%data(j), -1.0d0)) &
                            error stop 5_4
                    end do
                else
                    if ((b1(i)%id /= i) .or. (b1(i)%name /= 'b1')) &
                        error stop 6_4

                    do j = 1, 100
                        if (.not. precision_r8 (b1(i)%data(j), i*1.0d3+j*1.0d0))&
                            error stop 7_4
                    end do
                end if
            end do
        class default
            error stop 3_4
    end select
end

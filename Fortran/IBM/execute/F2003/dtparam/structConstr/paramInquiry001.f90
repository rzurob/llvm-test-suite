! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Use of the type parameter inquiry for the
!                               type parameters in structure constructors.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = (/(i, i=1, n)/)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name = 'default'
    end type

    type (child(8, 50, 20)) :: c_default
    type (child(4, 75, 25)) :: c_const

    parameter (c_default = child(8, 50, 20)(), &
               c_const = child(4,75,25)((/(i, i=75,1,-1)/), 'c_const'))
end module

module n
use m
    type container (k, n, l, m)
        integer, kind :: k, n
        integer, len :: l, m

        type (child(k, n, l)) :: data(m)
    end type

    type(container(c_default%k, c_default%n, c_default%l, 10)) :: co1 = &
        container(c_default%k, c_default%n, c_default%l, 10)(c_default)

    type (container(c_const%k, c_const%n, c_const%l, 6)) :: co2(10) = &
        container(c_const%k, c_const%n, c_const%l, 6)(data=c_const)
end module

program paramInquiry001
use n
    logical(4), external :: precision_r4, precision_r8

    !! verify co1 and co2
    do i = 1, 10
        if (co1%data(i)%name /= 'default') error stop 2_4

        do j = 1, 50
            if (.not. precision_r8(co1%data(i)%data(j), real(j, 8))) &
                    error stop 1_4
        end do

        do j = 1, 6
            if (co2(i)%data(j)%name /= 'c_const') error stop 3_4

            do k = 1, 75
                if (.not. precision_r4(co2(i)%data(j)%data(k), &
                    real(76-k, 4))) error stop 4_4
            end do
        end do
    end do
end

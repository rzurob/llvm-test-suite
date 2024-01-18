! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/23/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: Additional type parameters may be
!                               declared in the definition of the extended type
!                               Case: test arrays and array components
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, lb, ub)
        integer, kind :: k
        integer, len :: lb, ub

        integer(k) :: value (lb:ub)
        real(k) :: data
    end type

    type, extends(base) :: child (n)
        integer, kind :: n = 10     !<-- use this kind type-param in places of
                                    !<-- specification expression (purpose)

        logical flag(n)
        character(n) name
    end type

    logical(4) precision_r4, precision_r8

    external precision_r4, precision_r8

    type (child(8, lb=-1, ub=2)) c1_m(10)   !<-- n=10 by default
end module

program dtparamExtends006
use m
    type (child(4, lb=0, ub=6, n=20)) c1(2, 3)


    !! assign values to the data components
    c1_m%data = (/(i*1.2d0, i = 1, 10)/)
    c1_m%name = (/character(7):: 'c1_m 1', 'c1_m 2', 'c1_m 3', 'c1_m 4', 'c1_m 5', &
                  'c1_m 6', 'c1_m 7', 'c1_m 8', 'c1_m 9', 'c1_m 10'/)


    do i = 1, 10
        c1_m(i)%value = (/(i*10+j, j=c1_m%lb, c1_m%ub)/)
        c1_m(i)%flag = .true.
    end do

    c1%data = reshape((/(i*1.2e0, i = 1, 6)/), (/2,3/))
    c1%name = reshape((/'c1 11', 'c1 21', 'c1 12', 'c1 22', 'c1 13', 'c1 23'/),&
                        (/2,3/))

    do j = 1, 3
        do i = 1, 2
            c1(i, j)%value = (/(i*100+10*j, k=c1(i,j)%lb, c1(i,j)%ub)/)
            c1(i, j)%flag = .true.
        end do
    end do


    !! verify the component values for the object components
    if (any (c1_m%name /= (/character(7)::'c1_m 1', 'c1_m 2', 'c1_m 3', 'c1_m 4', 'c1_m 5',&
            'c1_m 6', 'c1_m 7', 'c1_m 8', 'c1_m 9', 'c1_m 10'/))) error stop 1_4

    if (any (c1(1,:)%name /= (/'c1 11', 'c1 12', 'c1 13'/))) error stop 2_4
    if (any (c1(2,:)%name /= (/'c1 21', 'c1 22', 'c1 23'/))) error stop 3_4

    do i = 1, 10
        if (.not. precision_r8 (c1_m(i)%data, 1.2d0*i)) error stop 4_4

        if (any(c1_m(i)%value /= (/(i*10+j, j=-1, 2)/))) error stop 5_4
        if (size(c1_m(i)%flag) /= 10)  error stop 6_4
        if (.not. all (c1_m(i)%flag)) error stop 7_4
    end do

    k = 1
    do j = 1, 3
        do i = 1, 2
            if (.not. precision_r4 (c1(i,j)%data, 1.2e0*k)) error stop 8_4

            if (any (c1(i,j)%value /= (/(i*100+10*j, kk=0, 6)/))) error stop 9_4
            if (size(c1(i,j)%flag) /= 20) error stop 10_4

            if (.not. all (c1(i,j)%flag)) error stop 11_4
            k = k + 1
        end do
    end do

end

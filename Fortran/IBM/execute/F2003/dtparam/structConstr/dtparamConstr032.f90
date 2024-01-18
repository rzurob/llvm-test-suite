! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/13/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Rank one array reshaped before used for
!                               rank two array data component; correct use of
!                               data source of different rank
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, dim1, dim2)
        integer, kind :: k
        integer, len :: dim1, dim2

        real(k) :: data(dim1, dim2)
    end type
end module

program dtparamConstr032
use m
    type (base(8,:,:)), pointer :: b1

    real(4) r1(200)
    real(8) d1(0:199)

    logical(4), external :: precision_r4, precision_r8

    r1 = sqrt((/(i*1.0, i=1,200)/))

    d1 = sqrt((/(i*1.0d0, i=1,200)/))

    allocate (base(8,10,20) :: b1)

    !! the next 2 stmts both are illegal
    b1 = base(8,10,20)(reshape(r1, (/10,20/)))

    k = 1
    do j = 1, 20
        do i = 1, 10
            if (.not. precision_r4 (real(b1%data(i,j), 4), sqrt(k*1.0))) &
                error stop 1_4

            k = k + 1
        end do
    end do

    deallocate (b1)

    allocate (b1, source=base(8,40,5)(reshape(d1, (/40, 5/))))

    k = 1
    do j = 1, 5
        do i = 1, 40
            if (.not. precision_r8 (b1%data(i,j), sqrt(k*1.0d0))) error stop 2_4

            k = k + 1
        end do
    end do
end

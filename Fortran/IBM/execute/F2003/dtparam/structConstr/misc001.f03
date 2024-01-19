! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/23/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316599)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base!(k, n)

        real(8) :: data(20)
    end type

    type container!(k,n, m)

        type(base) :: data(10)
    end type
end module

program dtparamConstr008
use m
    type(container) :: co1

    logical(4), external :: precision_r8

    co1 = container((/(base((/(log(j*1.0d2+i), i=1,20)/)),&
            j=1,10)/))

    !! verify
    do i = 1, 10
        do j = 1, 20
            if (.not. precision_r8(co1%data(i)%data(j), log(i*1.0d2+j))) &
                    error stop 1_4
        end do
    end do
end

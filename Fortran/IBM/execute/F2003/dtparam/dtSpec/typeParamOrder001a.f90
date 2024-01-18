!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The type parameter order is specified in
!                               the derived-type-stmt, not by the appearence
!                               order the type parameters specified in the
!                               type-parameter-def-stmt.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base (k1, k2, l1, l2)
        integer, len :: l2, l1
        integer, kind :: k2, k1

        integer(k1) :: id(l1)
        real(k2) :: data(l2)
    end type

    type, extends(base) :: child (k3, l3)
        integer, len :: l3
        integer, kind :: k3

        complex(k3) :: cx(l3)
    end type

    type(base(4, 8, 10, 30)) :: b1_m(10)
end module

program typeParamOrder001a
use m
    type (child(8,4,:,:, 4,20)), allocatable :: c1

    do i = 1, 10
        b1_m(i)%id = (/((i-1)*100 + j, j=1, 10)/)

        b1_m(i)%data = (/(datan(j*1.0d0), j=1, 30)/)*i*1.55d40
    end do

    allocate (child(8,4, 30, 45, 4,20) :: c1)

    c1%id = int(log(b1_m(7)%data), 8)   !<-- all 30 elements are 94
    c1%data = (/(i*1.3e0, i=1,45)/)
    c1%cx = (/((i*1.0e0, i*1.0e1), i=1, 20)/)

    !! verify results via write
    do i = 1, 10
        write(*, '(10i7,/,5(6g15.8))') b1_m(i)
    end do

    write (*, 100) c1

100 format (3(8i8), 6i8, /, 5(9f10.4), /, 5(4('(',f10.4,';', f10.4,') ')))
end

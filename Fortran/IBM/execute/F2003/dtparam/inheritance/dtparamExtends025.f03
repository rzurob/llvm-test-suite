! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/13/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: parent type is renamed via use statement
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(l, k)
        integer, kind :: k
        integer, len  :: l

        character(len=l) name
        integer(kind=k) id
    end type
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child (k2, l2)
        integer, kind :: k2
        integer, len :: l2

        complex(kind=k2) cx
        real (max(k, k2)) data(min(l, l2):max(l, l2))
    end type

    type (child(k=4, l=20, k2=8, l2=10)) c1_m(2)
end module

program dtparamExtends025
use m1
    !! make sure the parent component is accessible by name of base
    if ((c1_m%newbase%k /= 4) .or. (c1_m%newbase%l /= 20))  error stop 1_4
    if ((kind(c1_m%newbase%id) /= 4) .or. (len(c1_m%newbase%name) /= 20)) error stop 2_4

    call updateC1_m

    call printC1_m
end

subroutine updateC1_m
use m
use m1, only: c1_m
    c1_m%newbase = (/base(20, 4)('c1_m 1', 10), base(k=4, l=20)('c1_m 2', 20)/)

    call updateC1_mExtended
end subroutine

subroutine updateC1_mExtended
use m1, only: c1_m
    c1_m%cx = (/(1.35d53, 2.1d100), (2.24d-100, 1.32d55)/)
    c1_m(1)%data = (/(i*2.0d102, i=10, 20)/)
    c1_m(2)%data = 1.0d104 - c1_m(1)%data
end subroutine

subroutine printC1_m
use m1, only: c1_m

100 format (a, i10)
200 format ("(", g12.4, ', ', g12.4, '); ', /, 11g12.4)

    write (*, 100) c1_m(1)%newbase
    write (*, 200) c1_m(1)%cx, c1_m(1)%data

    print *, ''
    write (*, 100) c1_m(2)%newbase
    write (*, 200) c1_m(2)%cx, c1_m(2)%data
end subroutine

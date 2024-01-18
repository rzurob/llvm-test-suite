!#######################################################################
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
    type (container(8)), allocatable :: co1(:)

    type (child(8,:,:,:)), allocatable, target :: c1(:)

    allocate (child(8,35, 11,20) :: c1(20))

    allocate (co1(20))

    co1(1:5) = (/(container(8)(c1(i::2)%base), i=1,5)/)

    co1(6:10) = (/(container(8)(c1(2*i)%comp), i=6,10)/)

    co1(11:20) = (/(container(8)(c1(i::2)), i=1,10)/)

    !! verify the association status for co1(i)%data
    do i = 1, 5
        if (associated(co1(i)%data, c1(i::2))) error stop 1_4
        if (associated(co1(i)%data, co1(10+i)%data)) error stop 11_4

        if (.not. associated(co1(i)%data, c1(i::2)%base)) error stop 2_4
    end do

    if (.not. associated(co1(5)%data, co1(3)%data(2:))) error stop 3_4
    if (.not. associated(co1(5)%data, co1(1)%data(3:))) error stop 4_4
    if (.not. associated(co1(3)%data, co1(1)%data(2:))) error stop 5_4


    if (.not. associated(co1(4)%data, co1(2)%data(2:))) error stop 6_4

    do i = 6, 10
        if (.not. associated(co1(i)%data, c1(i*2)%comp)) error stop 7_4
    end do
end

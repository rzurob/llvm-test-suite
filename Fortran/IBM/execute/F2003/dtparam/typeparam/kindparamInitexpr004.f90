!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 12/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameters used in the kind type
!                               parameters for the components: pointer
!                               components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k)
        integer, kind :: k = 4

        type (base(k)), pointer :: next => null()
        type (base(8)), pointer :: data8 => null()
        type (base(2)), pointer :: data2 => null()
        type (base(k*10)), pointer :: data10k => null()
    end type
end module

program kindparamInitexpr004
use m
    type(base), pointer :: node1
    type(base(3)), pointer :: node2

    allocate (node1, node2)

    allocate (node1%data8, node2%data2)

    node2%data8 => node1%data8
    node1%data2 => node2%data2

    allocate(node1%next, node2%next)

    allocate (node2%next%data2, node2%next%data8)

    node1%next%data8 => node2%next%data8
    node1%next%data2 => node2%next%data2

    allocate (node1%data10k)
    allocate (node2%data10k)

    node1%next%data10k => node1%data10k
    node2%next%data10k => node2%data10k

    !! verify the results
    if (.not. associated(node1%data8, node2%data8)) error stop 1_4
    if (.not. associated(node1%data2, node2%data2)) error stop 2_4

    if (.not. associated(node1%next%data8, node2%next%data8)) error stop 3_4
    if (.not. associated(node1%next%data2, node2%next%data2)) error stop 4_4

    if (.not. associated(node1%data10k, node1%next%data10k)) error stop 5_4
    if (.not. associated(node2%data10k, node2%next%data10k)) error stop 6_4

    if ((node1%next%k /= 4) .or. (node1%data10k%k /= 40))   error stop 7_4
    if ((node2%next%k /= 3) .or. (node2%data10k%k /= 30))   error stop 8_4
end

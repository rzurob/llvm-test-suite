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
!*  DATE                       : 12/21/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: Linked list of unlimited poly-pointer
!                               data; test that the parameterized derived type
!                               entities can be used in the target of a pointer
!                               assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type objNode
        class(*), pointer :: data(:) => null()
        type (objNode), pointer :: next => null()
    end type
end module

module n
    type base(k, l)
        integer, kind :: k
        integer, len :: l

        integer(k) :: id = -1
        character(l) :: name = 'default'
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        real(k) :: data(n) = 1.0
    end type
end module

program dtparamExtends034
use m
use n
    type (objNode), pointer :: list, newNode, iterator

    class(base(4, 20)), pointer :: b1(:)
    class(base(8,:)), pointer :: b2(:)
    type (child(8, 20, 10)), target :: c1(10)

    allocate (b1(20))
    allocate (child(8, 16, 20):: b2(15))

    !! add a few nodes to list
    allocate(list)

    newNode => list
    newNode%data => b1

    allocate (newNode%next)
    newNode => newNode%next
    newNode%data => b2

    allocate (newNode%next)
    newNode => newNode%next
    newNode%data => b2(::2)

    allocate (newNode%next)
    newNode => newNode%next
    newNode%data => c1

    !! verify that there are 4 nodes in the list
    icount = 0

    iterator => list

    do while (associated(iterator))
        if (associated(iterator%data)) icount = icount + 1

        iterator => iterator%next
    end do

    if (icount /= 4) error stop 1_4

    !! now verify each node's association status.
    iterator => list
    if (.not. associated(iterator%data, b1)) error stop 2_4

    iterator => iterator%next
    if (.not. associated(iterator%data, b2)) error stop 3_4

    if (.not. associated(iterator%next%data, iterator%data(::2))) error stop 4_4

    iterator => iterator%next%next

    if (.not. associated(iterator%data, c1)) error stop 5_4

    iterator => iterator%next

    if (associated (iterator)) error stop 6_4
end

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/05/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: a linked-list and deferred type
!                               param.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program lenparamSpecexpr008
    type A (l)
        integer, len :: l

        type (A(l)), pointer :: next => null()
        integer :: i(l) = -1
    end type

    type(A(:)), pointer :: a1

    type(A(10)), target :: a2
    type(A(20)), target :: a3

    a1 => a2    !<-- at this point, a1%l is 10

    a1%i = (/(i, i = 1, 10)/)
    allocate (a1%next)

    a1 => a1%next
    a1%i = (/(11-i, i=1, 10)/)


    !!
    a1 => a3    !<-- at this point, a1%l is 20

    a1%i = (/(i, i = 1, 20)/)
    allocate (a1%next)

    a1 => a1%next
    a1%i = (/(21-i, i=1,20)/)

    allocate (a1%next)
    a1 => a1%next

    a1%i = (/(10*i, i=1, 20)/)


    !! now print out the values
    a1 => a2

    do while (associated(a1))
        print *, a1%i

        a1 => a1%next
    end do

    a1 => a3

    do while (associated(a1))
        print *, a1%i

        a1 => a1%next
    end do
end

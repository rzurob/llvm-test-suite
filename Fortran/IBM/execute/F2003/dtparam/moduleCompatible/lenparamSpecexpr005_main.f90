!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/05/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: expressions for the bounds of array
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program lenparamSpecexpr005
use m
    integer, parameter :: i_const = 10

    type (base (3, i_const)) b1
    type (base (1+i_const, 2*i_const)), allocatable :: b2(:)
    type (base (i_const/2, 2)) b3(10)           !<-- zero-size array component

    allocate (b2(100))

    print *, shape(b1%val), shape(b2(20)%val), shape (b3(1)%val)

    b1%val = reshape ((/(i, i=1, 9)/), (/3,3/))

    b2(2)%val = reshape ((/(i*2, i = 1, 11)/), (/11, 1/))

    b2(3)%val = b2(2)%val + 1

    b2(10:9:-1) = b2(2:3)

    !! verify results

    print *, b1%val(1,:)

    print *, b2(9)
    print *, b2(10)%val((/2, 3, 5, 7/), 1)
end

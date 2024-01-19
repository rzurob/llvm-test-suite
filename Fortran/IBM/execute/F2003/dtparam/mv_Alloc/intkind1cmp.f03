! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are component of DTs,
!*                               of type integer(1)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type A ( k1)
       integer, kind :: k1
       integer(k1), allocatable :: i1(:)
   end type

   type, extends(A) :: B(k2)
       integer, kind :: k2 = k1
       integer(k2), allocatable :: i2(:)
   end type

end module

use m
    type(A(1))  a1
    class(A(1)), allocatable :: a2

    allocate( a2, source = B(1,1)( (/ ( i, i = 1, 10) /), (/-8,-6,-4/) ) )

    select type (a2)
        type is (B(1,1))
            call move_alloc(a2%i2, a1%i1)
        class default
            stop 21
    end select
    print *, a1%i1
end

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of nonpoly type child
!*                               TO is of poly type base
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   type A ( k1, k2)
       integer, kind :: k1
       integer, kind :: k2 = k1
       integer(k1+k2), allocatable :: id
   end type

   type, extends(A) :: B(k3)
       integer, kind :: k3
       real(k3) :: r
   end type

   type(B(4,4,4)),  allocatable :: a2
   class(A(4,4)), allocatable :: a1
   logical precision_r4

   allocate(a2, source = B(4,4,4)(21, 11.0) )

   call move_alloc(a2, a1)

   select type (a1)
       type is (B(4,4,4))
           if ( a1%id /= 21 ) stop 21
           if ( .not. precision_r4( a1%r, 11.0 ) ) error stop 23_4
       class default
           stop 25
   end select
end

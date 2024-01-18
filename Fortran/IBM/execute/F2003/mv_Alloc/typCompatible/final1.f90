! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are polymorphic,
!*                               TO is of type parent, FROM of type child
!*                               FROM and TO are components of type
!*                               Check if final subroutine is called
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type ::  base
       contains
           final  :: final1
   end type

   type, extends(base) :: A
       class(base), allocatable ::  l1(:)
       contains
           final  :: final2
   end type

   type, extends(A) ::  B
       class(A), allocatable :: l2(:)
   end type

   contains
       subroutine final1(arg)
           type(base), intent(in) :: arg(:)
           print *, "This is final subroutine for type base"
       end subroutine

       subroutine final2(arg)
           type(A), intent(in) :: arg(:)
           print *, "This is final subroutine for type A"
       end subroutine
end module

program main
use m
   type(B), allocatable :: b1, b2(:)

   allocate(b1)
   allocate(b1%l2(2))
   allocate(b1%l2(2)%l1(50))

   allocate(b2(2))
   allocate(b2(1)%l2(3) )

   call move_alloc( b2(1)%l2, b1%l2(2)%l1 )

   if ( .not. allocated(b1%l2(2)%l1) ) stop 31
   if ( allocated(b2(1)%l2)) stop 32

   select type (x => b1%l2(2)%l1)

       type is (A)
           if ( size(x) /= 3 ) stop 41
       class default
           stop 51
   end select

end

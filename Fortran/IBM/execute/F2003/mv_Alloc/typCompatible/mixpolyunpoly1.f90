! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is polymorphic,
!8				 TO is unlimited polymorphic
!*                               FROM is component of child
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

       integer id
       contains
           final  :: final1
   end type

   type, extends(base) :: A
       class(base), allocatable ::  l1(:)
       contains
           final  :: final2
   end type

   type, extends(A) ::  B
       class(A), allocatable :: l2
   end type

   contains
       subroutine final1(arg)
           type(base), intent(in) :: arg
           print *, "This is final subroutine for type base"
       end subroutine

       subroutine final2(arg)
           type(A), intent(in) :: arg
           print *, "This is final subroutine for type A"
       end subroutine
end module

program main
use m
   type(B), allocatable :: b1
   class(*), allocatable :: b2(:)
   integer i

   allocate(b1)
   allocate(b1%l2)
   allocate(b1%l2%l1(2:7), source =(/ (base(i), i = 101, 106) /) )

   allocate(b2(2), source = (/ (1.0d2,1.4d1), (1.0d2, 1.4d1) /))

   call move_alloc( b1%l2%l1, b2 )

   if ( allocated(b1%l2%l1) ) error stop 31
   if ( .not. allocated(b2)) error stop 32

   select type (b2)

       type is (base)
           if ( size(b2) /= 6 ) error stop 41
           if ( lbound(b2,1) /= 2 ) error stop 42
           if ( ubound(b2,1) /= 7 ) error stop 45
           print *, b2%id
       class default
           stop 51
   end select

end


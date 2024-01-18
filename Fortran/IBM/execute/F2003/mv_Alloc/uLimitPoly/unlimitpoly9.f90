! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/30/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               TO and FROM are component of a derived-type
!*                               which is a allocatable subobject of another
!*                               derived-type
!*                               dynamic type is a derived-type
!*                               FROM is zero-size array
!*				 TO is finalized, rank one
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
   integer :: bNum = 0
   integer :: cNumrZero = 0
   integer :: cNumrOne = 0

   type base
       character(7)  name
       contains
         final:: final1
   end type

   type, extends(base) :: child
       contains
         final :: final2
	 final :: final3
   end type

   type A
       class(*), allocatable ::  l1(:)
   end type

   type B
       type(A), allocatable :: l2(:,:)
   end type

   contains
       subroutine final1(arg)
          type(base), intent(in) :: arg
          print *, "finalization for base"
	  bNum = bNum + 1
       end subroutine
       subroutine final2(arg)
          type(child), intent(in) :: arg
          print *, "finalization for child scalar"
	  cNumrZero = cNumrZero + 1
       end subroutine
       subroutine final3(arg)
          type(child), intent(in) :: arg(:)
          print *, "finalization for child rank one"
	  cNumrOne = cNumrOne + 1
       end subroutine

end module

program main
use m
   type(B) b1
   class(base), allocatable :: bs

   allocate(b1%l2(2,1))
   if ( .not. allocated(b1%l2) ) error stop 20

   ! allocate zero size array
   allocate( base :: b1%l2(1,1)%l1(6:5))
   if ( .not. allocated(b1%l2(1,1)%l1) ) error stop 30


   allocate( b1%l2(2,1)%l1(2:5), source = (/ (child('FORTRAN'), i = 1,4) /) )
   if ( .not. allocated(b1%l2(2,1)%l1) ) error stop 40

   cNumrZero = 0
   cNumrOne = 0
   bNum = 0

   call move_alloc( b1%l2(1,1)%l1, b1%l2(2,1)%l1 )

   if ( cNumrOne /= 1 ) error stop 41
   if ( cNumrZero /= 0 ) error stop 43
   if ( bNum /= 4 ) error stop 45

   if ( allocated(b1%l2(1,1)%l1) ) error stop 50
   if ( .not. allocated(b1%l2(2,1)%l1) ) error stop 60

   if ( same_type_as(bs, b1%l2(2,1)%l1) .neqv. .true. ) error stop 69

   if ( size(b1%l2(2,1)%l1) /= 0 ) error stop 70

end


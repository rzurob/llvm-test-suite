! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpoly8.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/30/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic, scalar
!*                               TO and FROM are component of a derived-type 
!*                               which is a allocatable subobject of another
!*                               derived-type
!*                               dynamic type is a derived-type 
!*                               when TO is deallocated, final subroutines for
!*                               its declared type and its parent type should
!*                               be called.
!*				 
!*				 TO is finalized; FROM is not finalized	
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   integer ::  bNum = 0
   integer :: cNum = 0
 
   type base
       character(7)  name
 
       contains
         final:: final1
   end type

   type, extends(base) :: child
       contains
         final :: final2
   end type 

   type A
       class(*), allocatable ::  l1
   end type

   type B
       type(A), allocatable :: l2(:)
   end type

   contains
       subroutine final1(arg)
          type(base), intent(in) :: arg
          print *, "finalization for base"
          
	  bNum = bNum + 1
       end subroutine
       subroutine final2(arg)
          type(child), intent(in) :: arg
          print *, "finalization for child"

	  cNum = cNum + 1
       end subroutine

end module

program main
use m

   type(B) b1

   allocate(b1%l2(2))
   if ( .not. allocated(b1%l2) ) stop 20

   allocate( b1%l2(1)%l1, source = base('Fortran'))
   if ( .not. allocated(b1%l2(1)%l1) ) stop 30

   allocate( b1%l2(2)%l1, source = child('FORTRAN'))
   if ( .not. allocated(b1%l2(2)%l1) ) stop 40 

   bNum = 0
   cNum = 0

   call move_alloc( b1%l2(1)%l1, b1%l2(2)%l1 )

   if ( bNum /= 1) stop 41
   if ( CNum /= 1) stop 43

   if ( allocated(b1%l2(1)%l1) ) stop 50
   if ( .not. allocated(b1%l2(2)%l1) ) stop 60 

   select type ( a => b1%l2(2)%l1 )
       type is (base)
            if ( a%name /= 'Fortran' ) stop 70
       class default
           stop 90 
   end select
      
end


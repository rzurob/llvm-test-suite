!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocDiagArraySect01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 3 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. FROM AND TO ARE ARRAY SECTION 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer :: i=1
   end type
end module

program move_allocDiagArraySect01

   use m
   integer,allocatable :: i1(:),i2(:)
   type(dtp(2,4)),allocatable :: dtp1(:),dtp2(:)
   type(dtp(2,:)),allocatable :: dtp3(:),dtp4(:)

   allocate(i1(4),i2(4))
   allocate(dtp1(4),dtp2(4))

   call move_alloc(i1(2:3),i2(1:2))
   call move_alloc(dtp1(2:3),dtp1(2:3))
   call move_alloc(dtp3(:),dtp4(:))
   call sub1(dtp1(1:2),dtp2(:))
   call sub2(dtp3(:),dtp4(2:2))
contains
   subroutine sub1(from,to)
         type(dtp(2,4)),allocatable :: from(:)
         type(dtp(2,4)),allocatable :: to(:)

         call move_alloc(from(:),to(:))
   end subroutine

   subroutine sub2(from,to)
         type(dtp(2,:)),allocatable :: from(:)
         type(dtp(2,:)),allocatable :: to(:)

         call move_alloc(from(:),to(:))
   end subroutine
end program

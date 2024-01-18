!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullGeneric04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 23 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD]) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88 
!* 2. NULL([MOLD])
!* 3. MOLD IS ALLOCATABLE 
!* 4. NULL(MOLD) IS USED AS ACTUAL ARGUMENT OF GENERIC TYPE-BOUND PROCEDURE 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k=8
      integer,len  :: l=20
      
      integer(k)   :: i=99
      contains
         procedure,nopass :: sub1
         procedure,nopass :: sub2 
         procedure,nopass :: sub3
         generic :: sub => sub1,sub2,sub3
   end type

   contains

   subroutine sub1(dt)
       type(dtp(2,:)),allocatable      :: dt(:)

         print *,"in sub1"
         if(allocated(dt)) then
            print *,"dtp is allocated"
            print *,dt%k,dt%l,dt%i
         else
            print *,"dtp is not allocated"
         endif

    end subroutine

    subroutine sub2(dt)
         type(dtp(4,:)),allocatable  :: dt(:)

         print *,"in sub2"
         if(allocated(dt)) then
            print *,"dtp is allocated"
            print *,dt%k,dt%l,dt%i
         else
            print *,"dtp is not allocated"
         endif
 
    end subroutine

    subroutine sub3(dt1,dt2)
         type(dtp(2,:)),allocatable  :: dt1(:)
         type(dtp(4,:)),allocatable  :: dt2(:)

         print *,"in sub3"
         if(allocated(dt1)) then
            print *,"dtp1 is allocated"
            print *,dt1%k,dt1%l,dt1%i
         else
            print *,"dtp1 is not allocated"
         endif
         if(allocated(dt2)) then
            print *,"dtp2 is allocated"
            print *,dt2%k,dt2%l,dt2%i
         else
            print *,"dtp2 is not allocated"
         endif
    end subroutine 
end module

program nullGeneric04
   use m
   implicit none

   integer :: j

   type(dtp)  :: obj 

   type(dtp(2,3))  :: tar1(3)=(/ (dtp(2,3)(j),j=1,3) /)
   type(dtp(4,6))  :: tar2(3)=(/ (dtp(4,6)(j),j=11,13) /)
   type(dtp(2,:)),allocatable :: dtp1(:)
   type(dtp(4,:)),allocatable :: dtp2(:)

   print *,"1---"
   call obj%sub(dtp1)
   call obj%sub(dtp2)
   call obj%sub(dtp1,dtp2)

   dtp1=tar1
   dtp2=tar2

   print *,"2---"
   call obj%sub(dtp1)
   call obj%sub(dtp2)
   call obj%sub(dtp1,dtp2)
  
   print *,"3---" 
   call obj%sub(null(dtp1))
   call obj%sub(null(dtp2))
   call obj%sub(null(dtp1),null(dtp2))
end program


!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsScalarProcPtrComp02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 16 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS SCALAR
!*  3. IF SOURCE IS SCALAR, EACH ELEMENT OF THE RESULT HAS A VALUE EQUAL TO SOURCE
!*  4. IF SOURCE IS SCALAR,THE SHAPE OF RESULT IS (MAX(NCOPIES,0)
!*  5. SOURCE IS POLYMORPHIC TYPE,PROCEDURE POINTER POINTS TO SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type base(l1)
      integer,len :: l1
      character(l1) :: c1
      procedure(sub1),nopass,pointer :: ptr1=>null()
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2
      character(l1+l2) :: c2
      procedure(sub2),nopass,pointer  :: ptr2=>null()
   end type

   contains

      subroutine sub1(int)
         integer,intent(inout) :: int

         print *," in sub1" 
         int=99
      end subroutine

      subroutine sub2(int)
         integer,intent(inout) :: int
     
         print *,"in sub2"
         int=-99
      end subroutine

end module

program spreadSourceIsScalarProcPtrComp02
  use m
  implicit none

  integer :: i,arg

  class(base(:)),pointer :: b1=>null()
  class(base(:)),pointer :: b2(:)=>null()

  allocate(child(2,3) :: b1)
  b1%c1="xlf"
  b1%ptr1=>sub1
  select type(b1)
     type is(child(*,*))
        b1%c2="fortran"
        b1%ptr2=>sub2
     class default
       error stop 100_4
  end select
  allocate(b2(3:5),source=spread(b1,1,3))
  select type(b2)
     type is(child(*,*))
       do i=3,5
         if(b2(i)%l1 /= 2)                                   error stop 10_4
         if(b2(i)%l2 /= 3)                                   error stop 11_4
         if(b2(i)%c1 /= "xl")                                error stop 12_4
         if(b2(i)%c2 /= "fortr")                             error stop 13_4
         if(.not. associated(b2(i)%ptr1,sub1))               error stop 14_4
         if(.not. associated(b2(i)%ptr2,sub2))               error stop 15_4

         arg=1

         call b2(i)%ptr1(arg)
         if(arg /= 99)                                       error stop 16_4
         
         call b2(i)%ptr2(arg)
         if(arg /= -99)                                      error stop 17_4 
       end do
      class default
         error stop 101_4
  end select   
end program

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPolyMis02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 12 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  12.4.1.7 :
!*  While an entity is associated with a dummy argument, following restrictions hold:
!*  1) Action that affects the allocation status of the entity or a subobject thereof shall be taken throgh the dummy argument. Action that affects the value of the entity or any subobject of it shall be taken only through the dummy argument unless:
!* a) the dummy argument has the POINRER attribute or
!* b) the dummy argument has TARGET attribute, the dummy argument does not have INTENT(IN), the dummy argument is a scalar object or an assumed shape array, and the actual argument is a target other than an array section with a vector subscript. 
! dummy argument in following test case has POINTER attribute.  
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1,l2)
      integer,len      :: l1,l2
      character(l1+l2) :: c 
   end type

   interface
     subroutine set(arg1,arg2)
        import
        type(dtp(:,:)),pointer,intent(inout) :: arg1(:)
        type(dtp(1,2)),intent(in) :: arg2
     end subroutine
   end interface

   integer,parameter :: N=4

   contains

      subroutine outer
         type(dtp(:,:)),pointer :: dtp1(:)=>null()
         type(dtp(:,:)),pointer :: dtp2(:)=>null()

            allocate(dtp1(2:N),source=&
                   [dtp(1,2)("000"),dtp(1,2)("111"),dtp(1,2)("222")] )

            dtp2(4:)=>dtp1 ! dtp2 is poiner

            call inner(dtp1) ! actual argument dtp1 is pointer too

            print *,dtp1            
            print *,dtp2

            contains
 
               subroutine inner(arg)
                   type(dtp(:,:)),pointer :: arg(:)

                   call set(arg,dtp(1,2)("123"))

                   if(associated(dtp1)) dtp1(3)=dtp(1,2)("456")

                   if(associated(arg)) arg(ubound(arg,1))=dtp(1,2)("789")

               end subroutine
         
      end subroutine 
end module

program dummyArgDeferNonPolyMis02
  use m
  implicit none

  call outer  
  
end program

subroutine set(arg1,arg2)
   use m,only : dtp
   type(dtp(:,:)),pointer,intent(inout) :: arg1(:)
   type(dtp(1,2)),intent(in) :: arg2

   if(associated(arg1)) arg1(lbound(arg1,1))= arg2
   
end subroutine

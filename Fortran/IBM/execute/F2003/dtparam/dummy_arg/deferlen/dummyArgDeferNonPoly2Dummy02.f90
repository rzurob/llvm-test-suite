!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPoly2Dummy02.f   
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
!*  1. 2 Dummy arguments
!*  2. first dummy argument : length type parameters are deferred
!*  3. second dummy argument: length type parameters are assumed
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1,l2)
      integer,len      :: l1,l2
      character(l1+l2) :: c 
   end type

   contains

   subroutine sub3(arg)
      type(dtp(:,:)),pointer,intent(inout) :: arg

        allocate(arg,source=dtp(3,6)("123456789"))
   end subroutine

   subroutine sub1(arg1,arg2)
        type(dtp(:,:)),pointer :: arg1
        type(dtp(*,*)),pointer,intent(in) :: arg2

        arg1=>arg2

        call sub2(arg1)

        contains 

           subroutine sub2(arg)
               type(dtp(:,:)),pointer,intent(inout) :: arg
                              
               allocate(arg,source=dtp(2,5)("xlf"//"test"))

               call sub3(arg)
                  
           end subroutine
   end subroutine

end module

program dummyArgDeferNonPoly2Dummy02
  use m
  implicit none

  type(dtp(:,:)),pointer :: dtp1=>null()
  type(dtp(1,2)), POINTER :: dtp2=>null()
  type(dtp(1,2)),target  :: tar1=dtp(1,2)("xlf")

  dtp2=>tar1

  call sub1(dtp1,dtp2)

  if(dtp1%l1 /= 3)                           stop 1
  if(dtp1%l2 /= 6)                           stop 2
  if(dtp1%c /= "123456789")                  stop 3

end program


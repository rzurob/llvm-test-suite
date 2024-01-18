!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullProcPtr01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 26 2008 
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
!* 3. INITIALIZE PROCEDURE POINTER WITH NULL()
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   interface
      function intface(arg)
          character(*) :: arg
          character(len(arg)) :: intface
      end function
   end interface 
   type dtp(k)
      integer,kind :: k
      procedure(intface),nopass,pointer :: procptr=>null()
   end type
 
   contains
      function fun(arg)
          character(*) :: arg
          character(len(arg)) :: fun
          fun=arg
      end function
       
end module

program nullProcPtr01
   use m
   implicit none

   type(dtp(4)) :: dtp1

   if(associated(dtp1%procptr))                        error stop 10_4

   dtp1=dtp(4)(fun)

   if(.not. associated(dtp1%procptr,fun))              error stop 11_4

   dtp1%procptr=>null()
   
   if(associated(dtp1%procptr))                        error stop 12_4

   dtp1=dtp(4)(funptr(fun))
  
   if(.not. associated(dtp1%procptr,fun))              error stop 13_4

   dtp1%procptr=>null()
   
   if(associated(dtp1%procptr))                        error stop 14_4 
  
   contains
      function funptr(arg)
          procedure(intface)   :: arg
          procedure(intface),pointer :: funptr

          funptr=>arg
      end function
      
end program


!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362322.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 19 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. defect 362322
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type A(l1)
      integer,len  :: l1
      procedure(ifun),nopass,pointer :: iprocptr=>null() 
   end type
  contains
      integer function ifun(int)
        integer,intent(in) :: int

        ifun=int
      end function

      function afun(ta)                  
        type(A(4)),intent(in) :: ta
        type(A(4))  :: afun

        afun=ta
      end function

end module

module m2
use m1,XA=>A                             
   contains
      subroutine assignA1(this,dt)
         class(XA(4)),intent(inout) :: this
         class(XA(4)),intent(in)    :: dt

      end subroutine
end module

program d362322
use m2

end program

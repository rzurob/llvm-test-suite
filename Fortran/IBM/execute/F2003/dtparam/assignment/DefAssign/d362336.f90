!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362336.f   
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
!* 1. defect 362336
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len  :: l1
      contains
        procedure :: assignA                
        generic   :: assignment(=)=>assignA
   end type

   type B(l2)
      integer,len   :: l2

      type(A(3)) :: acomp
      procedure(afun),nopass,pointer :: aptr=>null()  
   end type

  contains
      function afun(ta)
        type(A(3)),intent(in) :: ta
        type(A(3))  :: afun
        afun=ta
      end function

      subroutine assignA(this,dt)
         class(A(*)),intent(inout) :: this
         class(A(3)),intent(in)    :: dt
      end subroutine

       subroutine assignB(this,dt)          
         class(B(2)),intent(inout) :: this
         class(B(2)),intent(in)    :: dt
      end subroutine
end module

program d362336
end program

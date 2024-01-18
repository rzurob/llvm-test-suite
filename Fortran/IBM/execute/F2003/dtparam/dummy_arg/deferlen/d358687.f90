!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d358687.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 10 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  DEFECT 358687
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
     integer,len   :: l1
     character(l1) :: c1
   end type
   type B(l2)
     integer,len  :: l2
     type(A(:)),allocatable :: a1 
   end type
end module

program d358687

  use m
  implicit none

  type(B(:)),allocatable :: b1
  type(A(3)) :: a1=A(3)("xlf")

  b1=B(3)(a1) 

  print *,b1%l2,b1%a1%l1,"|",b1%a1%c1,"|"

end program

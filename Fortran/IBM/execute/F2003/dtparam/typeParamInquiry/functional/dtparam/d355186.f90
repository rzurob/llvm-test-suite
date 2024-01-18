!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355186.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 19 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
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
!* 1. TEST SECTION 6.1.3 
!* 2. DEFECT 355186
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(l2)
      integer,len  :: l2
   end type

   type :: A(l1)
     integer,len  :: l1
     type(B(4*l1)) :: b  
   end type
end module

program d355186

  use m
  implicit none

  type(A(:)),allocatable :: a2
  allocate(A(2) :: a2)  
  print *,a2%l1,a2%b%l2

end

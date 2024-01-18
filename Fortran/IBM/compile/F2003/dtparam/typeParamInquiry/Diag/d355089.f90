!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355089.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 17 2008 
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. DEFECT 355089   
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(l1,l2)
      integer(2),len  :: l1
      integer(4),len  :: l2
      integer :: i(l2:l1)
   end type
end module

  program d355089 
  use m
  implicit none

  type(base(:,:)),allocatable :: b
  allocate(b,source=base(-3,-6)(i=0)) !assign value to zero-sized array 

end

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355616.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : September 01 2008 
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
!* 2. DEFECT 355616
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(l)
      integer(8),len  :: l
   end type

   contains

     function fun1(b)
        type(base(*)),intent(in) :: b
        type(base(:)),allocatable :: fun1
        allocate(base(2*b%l) :: fun1)
     end function
end module
  use m
  implicit none
  type(base(:)),allocatable :: b1
  allocate(b1,source=fun1(base(4)()))
  print *,b1%l
  if(allocated(b1)) deallocate(b1)
  b1=fun1(base(4)())                 
  print *,b1%l
end


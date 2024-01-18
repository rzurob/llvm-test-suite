!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp08_d353476.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 8 2007
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
!* 2. DEFECT 353476
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k,l)
     integer(1),kind :: k
     integer(1),len  :: l
     character(k%kind+kind(k)) :: c1
     character(l%kind+kind(l)) :: c2
   end type
end module

  program dtParameterInquiryScalarComp08_d353476
  use m
  implicit none

  type(base(1,1)) :: t
  print *,t%k%kind,kind(t%k)
  print *,t%l%kind,kind(t%l)
  print *,t%c1%len,t%c2%len   

  end

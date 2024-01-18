!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp04_d353357.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 11 2008 
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
!* 2. DEFECT 353357
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1)
       integer(1),kind   :: k1
       integer(kind(k1)) :: i1=kind(k1) 
       integer(k1%kind)  :: i2=k1%kind
   end type

end module

  program dtParameterInquiryScalarComp04_d353357
  use m
  implicit none

  type(base(2)) :: t
  print *,t%k1,t%k1%kind,kind(t%k1)
  print *,t%i1%kind,t%i2%kind
  print *,t%i1,t%i2
 
  end

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353684.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 14 2008 
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
!* 2. DEFECT 353684
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,k2)
     integer(1),kind :: k1=2
     integer(k1),kind :: k2=k1
     integer,dimension(k2%kind,kind(k2)) :: i=k2%kind+kind(k2)
   end type

end module

program d353684

  use m
  implicit none

  type(base)  :: t
  print *,ubound(t%i,1),ubound(t%i,2) 
  print *,t%i
  print *,shape(t%i)

end

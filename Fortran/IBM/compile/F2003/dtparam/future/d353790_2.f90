!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353790_2.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 28 2008 
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
!* 2. DEFECT 353790 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1)
      integer(1),kind :: k1=2
      integer(k1),pointer :: i
   end type
end module

program d353790_2
   use m
   implicit none

   type(base) ::t
   print *,t%i%kind,kind(t%i),t%k1
   allocate(integer(t%k1):: t%i)

end

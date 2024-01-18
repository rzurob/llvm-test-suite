!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353284.f   
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
!* 2. DEFECT 353284
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: t(k1,k2)
         integer,kind :: k1
         integer(k1*4),kind :: k2

         integer :: i=k2%kind 
   end type
end module

  use m
  implicit none

  type(t(1,1)) :: t

  if(t%k1 /= 1)                                         error stop 10_4
  if(t%k2 /= 1)                                         error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 4)       error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 4)       error stop 13_4
  if(t%i /= 4)                                          error stop 14_4
  if(t%i%kind /= kind(t%i) .or. t%i%kind /= 4)          error stop 15_4

  end


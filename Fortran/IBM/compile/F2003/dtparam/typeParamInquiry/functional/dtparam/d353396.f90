!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353396.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept 08 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. DEFECT 353396 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(l)
      integer,len :: l 
      integer(kind(1)) :: i1
      integer(kind(-1))  :: i2
   end type                        
end module

program d353396
   use m
   implicit none
   type(base(2)) :: t

   if (t%i1%kind /= kind(1)) stop 1
   if (t%i2%kind /= kind(1)) stop 2
end program   


!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dd355802.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : September 06 2008 
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
!* 2. DEFECT d355802
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(2),len   :: l
      character(l)  :: c
   end type
   contains
     subroutine printInfo(b)
        type(base(*)) ::b             
        print *,b%l
        print *,b%c%len, len(b%c),(b%c%len /= 5)
     end subroutine
end module

program d355802
  use m
  implicit none

  type(base(5)) ::b
  print *,b%l
  print *,b%c%len, len(b%c),(b%c%len /= 5) 
  call printInfo(b)
end


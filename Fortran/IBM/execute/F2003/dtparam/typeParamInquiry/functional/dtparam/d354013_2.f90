!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d354013_2.f   
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
!* 2. DEFECT d354013_2
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
      integer,len  :: l1=2
      character(l1)   :: c
   end type
end module

program d354013_2
  use m
  implicit none

  type(A(3)),target :: a1=A(3)(c='abc')
  type(A(:)),allocatable :: a2
  type(A(:)),pointer :: a4

  print *,a1%l1,a1%c%len,len(a1%c),(a1%c%len /= 3)
  a2=a1
  print *,a2%l1,a2%c%len,len(a2%c),(a2%c%len /= 3)
  a4=>a1
  print *,a4%l1,a4%c%len,len(a4%c),(a4%c%len /= 3)
end


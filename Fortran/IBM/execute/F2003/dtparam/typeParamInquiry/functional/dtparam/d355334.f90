!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355334.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 23 2008 
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
!* 2. DEFECT 355334
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
      integer,len  :: l1=2
      character(l1)   :: c(2) 
   end type
end module

program d355334
  use m
  implicit none

  type(A(3)),target :: a1=A(3)(c=['abc','efg'])
  type(A(:)),allocatable :: a2
  type(A(:)),pointer :: a4

  a2=a1
  print *,a2%l1,"|",a2%c(1),"|",(a2%c(1) /= 'abc')
  print *,a2%l1,"|",a2%c(2),"|",(a2%c(2) /= 'efg')
  a4=>a1
  print *,a4%l1,"|",a4%c(1),"|",(a4%c(1) /= 'abc')
  print *,a4%l1,"|",a4%c(2),"|",(a4%c(2) /= 'efg')

end


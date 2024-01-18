!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDiag02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 21 2008 
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
!* 2. A DEFFERED TYPE PARAMETER OF A POINTER THAT IS NOT ASSOCIATED OR OF
!*    AN UNALLOCATED ALLOCATABLE VARIABLE SHALL NOT BE INQUIRED ABOUT 
!* 3. DEFECT 355287
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(:),allocatable :: c1(:)
   character(:),pointer     :: c2
   type A(l1)
      integer,len :: l1
   end type
   type,extends(A) :: B(l2)
      integer,len :: l2
   end type
   
end module

program dtParameterInquiryDiag02
  use m
  implicit none
  
  character(:),allocatable :: c3(:)
  character(:),pointer     :: c4
  type(A(:)),allocatable   :: a1
  type(A(:)),pointer       :: a2(:)
  type(B(:,:)),allocatable :: b1(:)
  type(B(:,:)),pointer     :: b2  

  print *,c1%len
  print *,c2%len
  print *,c3%len
  print *,c4%len
  print *,a1%l1
  print *,a2%l1
  print *,b1%l1
  print *,b1%l2
  print *,b2%l1
  print *,b2%l2

end

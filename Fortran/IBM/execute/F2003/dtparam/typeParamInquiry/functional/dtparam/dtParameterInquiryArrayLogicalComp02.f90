!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayLogicalComp02.f   
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
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. ALLOCATABLE LOGICAL ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind :: k1
     integer(1),kind :: k2

     integer(2),len  :: l1
     integer(k1),len :: l2

     logical(1),allocatable :: a1(:)
     logical(2),allocatable :: a2(:)
     logical(4),allocatable :: a3(:)
     logical(8),allocatable :: a4(:)
     logical(k1),allocatable :: a5(:)
     logical(k2),allocatable :: a6(:)
     logical(k1+k2),allocatable :: a7(:)
     logical(k1%kind),allocatable :: a8(:)
     logical(k2%kind),allocatable :: a9(:)
     logical(k1%kind+k2%kind),allocatable :: a10(:,:)
     logical(1),allocatable :: a11(:)   
     logical(1),allocatable :: a12(:)     
     logical(1),allocatable :: a13(:)  
     logical(2),allocatable :: a14(:) 
     logical(max(4,k2)),allocatable :: a15(:)
     logical(ichar(char(4))),allocatable :: a16(:)

   end type 
         
end module

  program dtParameterInquiryArrayLogicalComp02
  use m
  implicit none

  type(base(2,2,4,1))  :: t

  if(t%k1 /= 2)                                             error stop 10_4
  if(t%k2 /= 2)                                             error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)           error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 1)           error stop 13_4

  if(t%a1%kind /= kind(t%a1) .or. t%a1%kind /= 1)           error stop 14_4
  if(t%a2%kind /= kind(t%a2) .or. t%a2%kind /= 2)           error stop 15_4
  if(t%a3%kind /= kind(t%a3) .or. t%a3%kind /= 4)           error stop 16_4
  if(t%a4%kind /= kind(t%a4) .or. t%a4%kind /= 8)           error stop 17_4 

  if(t%a5%kind /= kind(t%a5) .or. t%a5%kind /= 2)           error stop 18_4
  if(t%a6%kind /= kind(t%a6) .or. t%a6%kind /= 2)           error stop 19_4
  if(t%a7%kind /= kind(t%a7) .or. t%a7%kind /= 4)           error stop 20_4
  if(t%a8%kind /= kind(t%a8) .or. t%a8%kind /= 1)           error stop 21_4

  if(t%a9%kind /= kind(t%a9) .or. t%a9%kind /= 1)           error stop 22_4
  if(t%a10%kind /= kind(t%a10) .or. t%a10%kind /= 2)        error stop 23_4
  if(t%a11%kind /= kind(t%a11) .or. t%a11%kind /= 1)        error stop 24_4
  if(t%a12%kind /= kind(t%a12) .or. t%a12%kind /= 1)        error stop 25_4
  if(t%a13%kind /= kind(t%a13) .or. t%a13%kind /= 1)        error stop 26_4
  if(t%a14%kind /= kind(t%a14) .or. t%a14%kind /= 2)        error stop 27_4
  if(t%a15%kind /= kind(t%a15) .or. t%a15%kind /= 4)        error stop 28_4
  if(t%a16%kind /= kind(t%a16) .or. t%a16%kind /= 4)        error stop 29_4
   

  end

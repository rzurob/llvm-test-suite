!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayCharComp02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 15 2008 
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
!* 4. CHARACTER ALLOCATABLE ARRAY COMPONENT
!* 5. DEFECT 353685 353295
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     
      integer(1),kind :: k1=2
      integer(2),kind :: k2=4

      integer(k1),len :: l1=k1
      integer(k2),len :: l2=k2
      
      character(k1),allocatable :: c1(:)
      character(k2),allocatable :: c2(:)
      character(len=l1),allocatable :: c3(:,:)
      character(len=l2),allocatable :: c4(:)
      character(l1+l2),allocatable  :: c5(:)
      character(k2),allocatable,dimension(:) :: c6 !defect 353685
      character(k1+k2),allocatable,dimension(:) :: c7
      character(k1%kind),allocatable :: c8(:)
      character(k1%kind+k2%kind),allocatable :: c9(:,:)
      character(2),allocatable :: c10(:)
      character(l1%kind+l2%kind),allocatable :: c11(:) ! defect 353295
      character(k1+k2),allocatable :: c12(:) ! defect 353685
      character(l2%kind),allocatable :: c13(:)
      character(l1%kind),allocatable :: c14(:)

   end type 
         
end module

  program dtParameterInquiryArrayCharComp02
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 2)                                             error stop 10_4
  if(t%k2 /= 4)                                             error stop 11_4
  if(t%l1 /= 2)                                             error stop 12_4
  if(t%l2 /= 4)                                             error stop 13_4
  
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)           error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)           error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)           error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 4)           error stop 17_4

  if(t%c1%kind /= kind(t%c1) .or. t%c1%kind /= 1)           error stop 18_4
  if(t%c2%kind /= kind(t%c2) .or. t%c2%kind /= 1)           error stop 19_4
  if(t%c3%kind /= kind(t%c3) .or. t%c3%kind /= 1)           error stop 20_4
  if(t%c4%kind /= kind(t%c4) .or. t%c4%kind /= 1)           error stop 21_4
  if(t%c5%kind /= kind(t%c5) .or. t%c5%kind /= 1)           error stop 22_4
  if(t%c6%kind /= kind(t%c6) .or. t%c6%kind /= 1)           error stop 23_4
  if(t%c7%kind /= kind(t%c7) .or. t%c7%kind /= 1)           error stop 24_4
  if(t%c8%kind /= kind(t%c8) .or. t%c8%kind /= 1)           error stop 25_4
  if(t%c9%kind /= kind(t%c9) .or. t%c9%kind /= 1)           error stop 26_4
  if(t%c10%kind /= kind(t%c10) .or. t%c10%kind /= 1)        error stop 27_4
  if(t%c11%kind /= kind(t%c11) .or. t%c11%kind /= 1)        error stop 28_4
  if(t%c12%kind /= kind(t%c12) .or. t%c12%kind /= 1)        error stop 29_4
  if(t%c13%kind /= kind(t%c13) .or. t%c13%kind /= 1)        error stop 30_4
  if(t%c14%kind /= kind(t%c14) .or. t%c14%kind /= 1)        error stop 31_4

  if(t%c1%len /= len(t%c1) .or. t%c1%len /= 2)              error stop 32_4
  if(t%c2%len /= len(t%c2) .or. t%c2%len /= 4)              error stop 33_4
  if(t%c3%len /= len(t%c3) .or. t%c3%len /= 2)              error stop 34_4
  if(t%c4%len /= len(t%c4) .or. t%c4%len /= 4)              error stop 35_4
  if(t%c5%len /= len(t%c5) .or. t%c5%len /= 6)              error stop 36_4
  if(t%c6%len /= len(t%c6) .or. t%c6%len /= 4)              error stop 37_4
  if(t%c7%len /= len(t%c7) .or. t%c7%len /= 6)              error stop 38_4
  if(t%c8%len /= len(t%c8) .or. t%c8%len /= 1)              error stop 39_4
  if(t%c9%len /= len(t%c9) .or. t%c9%len /= 3)              error stop 40_4
  if(t%c10%len /= len(t%c10) .or. t%c10%len /= 2)           error stop 41_4
  if(t%c11%len /= len(t%c11) .or. t%c11%len /= 6)           error stop 42_4
  if(t%c12%len /= len(t%c12) .or. t%c12%len /= 6)           error stop 43_4
  if(t%c13%len /= len(t%c13) .or. t%c13%len /= 4)           error stop 44_4
  if(t%c14%len /= len(t%c14) .or. t%c14%len /= 2)           error stop 45_4


  end

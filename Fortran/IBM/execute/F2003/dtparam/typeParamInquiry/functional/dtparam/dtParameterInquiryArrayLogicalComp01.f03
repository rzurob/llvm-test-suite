!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 11 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. LOGICAL ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind :: k1
     integer(1),kind :: k2

     integer(2),len  :: l1
     integer(k1),len :: l2

     logical(1) :: a1(2)
     logical(2) :: a2(l2,l1)
     logical(4) :: a3(l1:l2)
     logical(8) :: a4(2:k1)

     logical(k1) :: a5(1)
     logical(k2) :: a6(2)
     logical(k1+k2) :: a7(k1+k2)
     logical(1) :: a8(max(l1,l2))
     logical(1) :: a9(1)
     logical(2) :: a10(k1:2)
     logical(1) :: a11(2)
     logical(1) :: a12(k1)
     logical(1) :: a13(k1+k2)
     logical(1+1) :: a14(1+1)
     logical(max(4,k2)) :: a15(l1:l1+l2)
     logical(4) :: a16(4)

   end type

end module

  program dtParameterInquiryArrayLogicalComp01
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

  if(lbound(t%a1,1) /=1 .or. ubound(t%a1,1) /= 2)           error stop 30_4
  if(lbound(t%a2,1) /=1 .or. ubound(t%a2,1) /= t%l2)        error stop 31_4
  if(lbound(t%a2,2) /=1 .or. ubound(t%a2,2) /= t%l1)        error stop 32_4

  if(lbound(t%a3,1) /=t%l1 .or. ubound(t%a3,1) /= t%l2)     error stop 33_4
  if(lbound(t%a4,1) /=2  .or. ubound(t%a4,1) /= 2)          error stop 34_4

  if(lbound(t%a5,1) /=1  .or. ubound(t%a5,1) /= 1)          error stop 35_4
  if(lbound(t%a6,1) /=1  .or. ubound(t%a6,1) /= 2)          error stop 36_4
  if(lbound(t%a7,1) /=1  .or. ubound(t%a7,1) /= 4)          error stop 37_4
  if(lbound(t%a8,1) /=1  .or. ubound(t%a8,1) /= 4)          error stop 38_4
  if(lbound(t%a9,1) /=1  .or. ubound(t%a9,1) /= 1)          error stop 39_4
  if(lbound(t%a10,1) /=2  .or. ubound(t%a10,1) /= 2)        error stop 40_4
  if(lbound(t%a11,1) /=1  .or. ubound(t%a11,1) /= 2)        error stop 41_4
  if(lbound(t%a12,1) /=1  .or. ubound(t%a12,1) /= 2)        error stop 42_4
  if(lbound(t%a13,1) /=1  .or. ubound(t%a13,1) /= 4)        error stop 43_4
  if(lbound(t%a14,1) /=1  .or. ubound(t%a14,1) /= 2)        error stop 44_4
  if(lbound(t%a15,1) /=4  .or. ubound(t%a15,1) /= 5)        error stop 45_4
  if(lbound(t%a16,1) /=1  .or. ubound(t%a16,1) /= 4)        error stop 46_4

  end
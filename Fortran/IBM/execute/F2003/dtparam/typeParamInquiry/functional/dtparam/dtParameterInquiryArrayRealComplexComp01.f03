!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 12 2008
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
!* 4. REAL,COMPLEX ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind :: k1=2
     integer(2),kind :: k2=4
     integer(2),len  :: l1=k1+k2
     integer(4),len  :: l2=k2-k1

     real(4),dimension(l1,l2) :: r1
     real(8)    :: r2(k1,k2)
     real(16)   :: r3(k1)
     real(2*k1) :: r4(l1:l2)
     real(k2)   :: r5(l1)
     real(4*k1%kind)  :: r6(3)
     real(4*k2%kind)  :: r7(4*k2%kind)
     real(k1*2+k2)    :: r8(l2:k2)
     real(kind(k1+k2)*4) :: r9(l1+l2)
     real(kind(111.11))  :: r10(3:6)

     complex(4)  :: x1(2)
     complex(8)  :: x2(l1)
     complex(16) :: x3(l1%kind)
     complex(2*k1) :: x4(l1%kind:l2%kind)
     complex(2*k2) :: x5(2:4)
     complex(2*k1+k2) :: x6(4)
     complex(k1*k2)   :: x7(k1*k2)
     complex(4*k2%kind)  ::x8(4*k2%kind)
     complex(8*kind(k1)) :: x9(k1%kind)
     complex(2*k1+k2)    :: x10(2*k1+k2)

   end type

end module

  program dtParameterInquiryArrayRealComplexComp01
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 2)                                             error stop 10_4
  if(t%k2 /= 4)                                             error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)           error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)           error stop 13_4

  if(t%l1 /=6)                                              error stop 14_4
  if(t%l2 /=2)                                              error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)           error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 4)           error stop 17_4

  if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /= 4)           error stop 18_4
  if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /= 8)           error stop 19_4
  if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /=16)           error stop 20_4
  if(t%r4%kind /= kind(t%r4) .or. t%r4%kind /= 4)           error stop 21_4
  if(t%r5%kind /= kind(t%r5) .or. t%r5%kind /= 4)           error stop 22_4
  if(t%r6%kind /= kind(t%r6) .or. t%r6%kind /= 4)           error stop 23_4
  if(t%r7%kind /= kind(t%r7) .or. t%r7%kind /= 8)           error stop 24_4
  if(t%r8%kind /= kind(t%r8) .or. t%r8%kind /= 8)           error stop 25_4
  if(t%r9%kind /= kind(t%r9) .or. t%r9%kind /= 8)           error stop 26_4
  if(t%r10%kind /= kind(t%r10) .or. t%r10%kind /= 4)        error stop 27_4


  if(t%x1%kind /= kind(t%x1) .or. t%x1%kind /= 4)           error stop 28_4
  if(t%x2%kind /= kind(t%x2) .or. t%x2%kind /= 8)           error stop 29_4
  if(t%x3%kind /= kind(t%x3) .or. t%x3%kind /=16)           error stop 30_4
  if(t%x4%kind /= kind(t%x4) .or. t%x4%kind /= 4)           error stop 31_4
  if(t%x5%kind /= kind(t%x5) .or. t%x5%kind /= 8)           error stop 32_4
  if(t%x6%kind /= kind(t%x6) .or. t%x6%kind /= 8)           error stop 33_4
  if(t%x7%kind /= kind(t%x7) .or. t%x7%kind /= 8)           error stop 34_4
  if(t%x8%kind /= kind(t%x8) .or. t%x8%kind /= 8)           error stop 35_4
  if(t%x9%kind /= kind(t%x9) .or. t%x9%kind /= 8)           error stop 36_4
  if(t%x10%kind /= kind(t%x10) .or. t%x10%kind /= 8)        error stop 37_4

  if(lbound(t%r1,1) /= 1 .or. ubound(t%r1,1) /= t%l1)       error stop 38_4
  if(lbound(t%r1,2) /= 1 .or. ubound(t%r1,2) /= t%l2)       error stop 39_4

  if(lbound(t%r2,1) /= 1 .or. ubound(t%r2,1) /= t%k1)       error stop 40_4
  if(lbound(t%r2,2) /= 1 .or. ubound(t%r2,2) /= t%k2)       error stop 41_4

  if(lbound(t%r3,1) /= 1 .or. ubound(t%r3,1) /= t%k1)       error stop 42_4

  if(lbound(t%r4,1) /=t%l1 .or. ubound(t%r4,1) /= t%l2)     error stop 43_4
  if(lbound(t%r5,1) /=1 .or. ubound(t%r5,1) /= t%l1)        error stop 44_4
  if(lbound(t%r6,1) /=1 .or. ubound(t%r6,1) /= 3)           error stop 45_4
  if(lbound(t%r7,1) /=1 .or. ubound(t%r7,1) /= 8)           error stop 46_4
  if(lbound(t%r8,1) /=t%l2 .or. ubound(t%r8,1) /=t%k2)      error stop 47_4
  if(lbound(t%r9,1) /=1 .or. ubound(t%r9,1) /=8)            error stop 48_4
  if(lbound(t%r10,1) /=3 .or. ubound(t%r10,1) /=6)          error stop 49_4

  if(lbound(t%x1,1) /= 1 .or. ubound(t%x1,1) /= 2)          error stop 50_4
  if(lbound(t%x2,1) /= 1 .or. ubound(t%x2,1) /= 6)          error stop 51_4
  if(lbound(t%x3,1) /= 1 .or. ubound(t%x3,1) /= 2)          error stop 52_4
  if(lbound(t%x4,1) /= 2 .or. ubound(t%x4,1) /= 4)          error stop 53_4
  if(lbound(t%x5,1) /= 2 .or. ubound(t%x5,1) /= 4)          error stop 54_4
  if(lbound(t%x6,1) /= 1 .or. ubound(t%x6,1) /= 4)          error stop 55_4
  if(lbound(t%x7,1) /= 1 .or. ubound(t%x7,1) /= 8)          error stop 56_4
  if(lbound(t%x8,1) /= 1 .or. ubound(t%x8,1) /= 8)          error stop 57_4
  if(lbound(t%x9,1) /= 1 .or. ubound(t%x9,1) /= 1)          error stop 58_4
  if(lbound(t%x10,1) /= 1 .or. ubound(t%x10,1) /= 8)        error stop 59_4

  end

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp04.f
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. DIFFERENT TYPE PARAMETER
!* 4. SCALAR REAL COMPONENT
!* 5. DEFECT 353357
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,k3,l1,l2,l3)
      integer(1),kind    :: k1
      integer(2*k1),kind :: k2
      integer(k1+k2),kind :: k3

      integer(k1),len :: l1
      integer(4),len  :: l2
      integer(k2),len :: l3

      real             :: r1
      real(2*k1)       :: r2
      real(k1+k2)      :: r3
      real(4)          :: r4
      real(4+kind(k2))  :: r5
      real(kind(1.0_4))   :: r6
      real(8)          :: r7
      real(4*4)        :: r8

   end type
end module

  program dtParameterInquiryScalarComp04
  use m
  implicit none

  type(base(2,2,1,8,12,1))  :: t

  if(t%k1 /= 2)                                         error stop 10_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /=1)        error stop 11_4
  if(t%k2 /= 2)                                         error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /=4)        error stop 13_4
  if(t%k3 /= 1)                                         error stop 14_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /=4)        error stop 15_4

  if(t%l1 /= 8)                                         error stop 16_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /=2)        error stop 17_4

  if(t%l2 /= 12)                                        error stop 18_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /=4)        error stop 19_4

  if(t%l3 /= 1)                                         error stop 20_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /=2)        error stop 21_4

  if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /=4)        error stop 22_4
  if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /=4)        error stop 23_4
  if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /=4)        error stop 24_4
  if(t%r4%kind /= kind(t%r4) .or. t%r4%kind /=4)        error stop 25_4
  if(t%r5%kind /= kind(t%r5) .or. t%r5%kind /=8)        error stop 26_4

  if(t%r6%kind /= kind(t%r6) .or. t%r6%kind /=4)        error stop 27_4
  if(t%r7%kind /= kind(t%r7) .or. t%r7%kind /=8)        error stop 28_4
  if(t%r8%kind /= kind(t%r8) .or. t%r8%kind /=16)       error stop 29_4

  end

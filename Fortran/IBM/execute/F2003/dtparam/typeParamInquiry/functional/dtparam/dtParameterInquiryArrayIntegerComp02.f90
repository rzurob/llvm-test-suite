!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 14 2008
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
!* 4. INTEGER ARRAY COMPONENT
!* 5. DEFECT 353684
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,k3,l1,l2)

     integer(1),kind  :: k1=2
     integer(k1),kind :: k2=k1
     integer(k1+k2),kind :: k3=kind(char(ichar('b')))

     integer(1),len  :: l1=max(1,2)
     integer(k1),len :: l2=k2

     integer(k3) :: i1(l1,l2)
     integer(2),dimension(k1,k1) :: i2=2*k1
     integer(kind(k1)) :: i3(k1%kind)
     integer(8) :: i4(k1:-1)
     integer(1) :: i6(-l1:l1)
     integer(k1+k2),dimension(2,2) :: i7
     integer(selected_int_kind(k1*k2*k3)) ::i8(selected_int_kind(k3*k2*k1))
     integer(k1*2),dimension(1:2) :: i9(ichar(char(32)))
   end type

end module

  program dtParameterInquiryArrayIntegerComp02
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 2)                                         error stop 10_4
  if(t%k2 /= 2)                                         error stop 11_4
  if(t%k3 /= 1)                                         error stop 12_4

  if(t%l1 /= 2)                                         error stop 13_4
  if(t%l2 /= 2)                                         error stop 14_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)       error stop 16_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)       error stop 17_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /= 4)       error stop 18_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)       error stop 19_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)       error stop 20_4

  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 1)       error stop 22_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 2)       error stop 23_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 1)       error stop 24_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 8)       error stop 25_4
  if(t%i6%kind /= kind(t%i6) .or. t%i6%kind /= 1)       error stop 27_4
  if(t%i7%kind /= kind(t%i7) .or. t%i7%kind /= 4)       error stop 28_4
  if(t%i8%kind /= kind(t%i8) .or. t%i8%kind /= 2)       error stop 29_4
  if(t%i9%kind /= kind(t%i9) .or. t%i9%kind /= 4)       error stop 30_4

  if(lbound(t%i1,1) /=1 .or. ubound(t%i1,1) /= t%l1)    error stop 31_4
  if(lbound(t%i1,2) /=1 .or. ubound(t%i1,2) /= t%l2)    error stop 32_4
  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 2)       error stop 33_4
  if(lbound(t%i2,2) /=1 .or. ubound(t%i2,2) /= 2)       error stop 34_4
  if(any(t%i2 /= 4))                                    error stop 35_4

  if(lbound(t%i3,1) /=1 .or. ubound(t%i3,1) /= 1)       error stop 36_4
  if(lbound(t%i4,1) /=2 .or. ubound(t%i4,1) /=-1)       error stop 37_4

  if(lbound(t%i6,1) /=-2 .or. ubound(t%i6,1) /= 2)      error stop 39_4
  if(lbound(t%i7,1) /=1 .or. ubound(t%i7,1) /= 2)       error stop 40_4
  if(lbound(t%i7,2) /=1 .or. ubound(t%i7,2) /= 2)       error stop 41_4
  if(lbound(t%i8,1) /=1 .or. ubound(t%i8,1) /= 2)       error stop 42_4
  if(lbound(t%i9,1) /=1 .or. ubound(t%i9,1) /= 32)      error stop 43_4

  end

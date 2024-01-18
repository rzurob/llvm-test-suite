!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 10 2008
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
!* 5. DEFECT 353612
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k,l)
     integer(1),kind  :: k=2
     integer(k),len   :: l=k
     integer(k+k) :: i1(k,k)=0
     integer(4),dimension(k,k) :: i2=0
     integer(k) :: i3(k,2)=0
     integer :: i4(k,2)=0
     integer,dimension(k) :: i5=0
   end type

end module

  program dtParameterInquiryArrayIntegerComp04
  use m
  implicit none

  type(base)  :: t

  if(t%k /= 2)                                           error stop 10_4
  if(t%l /= 2)                                           error stop 11_4
  if(t%k%kind /= kind(t%k) .or. t%k%kind /= 1)           error stop 12_4
  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 4)        error stop 13_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 4)        error stop 14_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 2)        error stop 15_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 4)        error stop 16_4
  if(t%i5%kind /= kind(t%i5) .or. t%i5%kind /= 4)        error stop 17_4

  if(lbound(t%i1,1) /=1 .or. ubound(t%i1,1) /= 2)        error stop 18_4
  if(lbound(t%i1,2) /=1 .or. ubound(t%i1,2) /= 2)        error stop 19_4

  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 2)        error stop 20_4
  if(lbound(t%i2,2) /=1 .or. ubound(t%i2,2) /= 2)        error stop 21_4

  if(lbound(t%i3,1) /=1 .or. ubound(t%i3,1) /= 2)        error stop 22_4
  if(lbound(t%i3,2) /=1 .or. ubound(t%i3,2) /= 2)        error stop 23_4

  if(lbound(t%i4,1) /=1 .or. ubound(t%i4,1) /= 2)        error stop 24_4
  if(lbound(t%i4,2) /=1 .or. ubound(t%i4,2) /= 2)        error stop 25_4

  if(lbound(t%i5,1) /=1 .or. ubound(t%i5,1) /= 2)        error stop 26_4

  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 2)        error stop 19_4
  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 2)        error stop 19_4

  end

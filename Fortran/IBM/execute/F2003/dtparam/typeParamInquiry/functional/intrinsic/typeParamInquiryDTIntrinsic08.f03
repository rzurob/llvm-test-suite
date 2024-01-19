!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 8 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. USE EXTENDS
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type first
      integer(2) :: i=5
      character(:),allocatable :: c1
   end type first
   type,extends(first) :: second
      character(:),pointer :: c2 => null()
   end type
   type,extends(second) :: third
      real :: r=1.0
   end type

end module

  program typeParamInquiryDTIntrinsic08
  use m
  implicit none

  character(len=3),target :: t="xlf"
  type(third)  :: dt

  allocate(dt%second%c2,source=t)
  allocate(dt%first%c1,source="test")


  if(dt%i%kind /=2 .or. dt%i%kind /= kind(dt%i)) error stop 10_4
  if(dt%r%kind /=4 .or. dt%r%kind /= kind(dt%r))  error stop 11_4

  if(dt%c1%len /= len(dt%c1) .or. dt%c1%len /= 4) error stop 12_4
  if(dt%c1%kind /= kind(dt%c1) .or. dt%c1%kind /= 1)  error stop 13_4

  if(dt%c2%len /= len(dt%second%c2) .or. dt%c2%len /= 3)  error stop 14_4
  if(dt%c2%kind /= kind(dt%c2) .or. dt%c2%kind /= 1)  error stop 15_4


end

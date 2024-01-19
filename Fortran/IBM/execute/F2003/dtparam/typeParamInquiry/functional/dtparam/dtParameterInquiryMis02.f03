!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 26 2008
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
!* 3. TYPE PARAMETER INQUIRY AS ARRAY BOUND
!* 4. DEFECT 354583
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
     integer(2),kind :: k=2
     integer(4),len  :: l=4
     character(:),pointer :: c(:)=>null()
   end type
end module

  program dtParameterInquiryMis02
  use m
  implicit none

  character(len=*),parameter :: c1="xlftest"
  character(len=8) :: c2(c1%len-1:c1%len+1)=c1
  character(:),allocatable :: c4(:)

  type(base(k=kind(c1)+c1%kind,l=len(c1)+2)) :: b1
  type(base),parameter :: b2=base()
  type(base(2,:)),allocatable :: b3(:)

  integer(8) :: i1(b1%k:b1%l)
  integer(2) :: i2(c1%kind:c1%len)


  if(lbound(i1,1) /= 2)                                   error stop 8_4
  if(ubound(i1,1) /= 9)                                   error stop 9_4


  allocate(b1%c(c1%kind:c1%len),source=c1(1:3))

  if(any(c2 /= "xlftest"))                                error stop 10_4
  if(c2%len /= len(c2) .or. c2%len /= 8)                  error stop 11_4
  if(lbound(c2,1) /= 6)                                   error stop 12_4
  if(ubound(c2,1) /= 8)                                   error stop 13_4

  if(any(b1%c /= "xlf"))                                  error stop 14_4
  if(b1%k /= 2)                                           error stop 15_4
  if(b1%l /= 9)                                           error stop 16_4
  if(b1%c%len /= len(b1%c) .or. b1%c%len /= 3)            error stop 17_4

  deallocate(b1%c)
  allocate(b1%c(b1%k:b1%l),source=c1(4:7))

  if(any(b1%c /= "test"))                                 error stop 18_4
  if(b1%c%len /= len(b1%c) .or. b1%c%len /= 4)            error stop 19_4

  allocate(c4(2*b1%k:2*b1%l),source=c1)

  if(any(c4 /= "xlftest"))                                error stop 20_4
  if(c4%len /= len(c4) .or. c4%len /= 7)                  error stop 21_4
  if(lbound(c4,1) /= 4)                                   error stop 22_4
  if(ubound(c4,1) /= 18)                                  error stop 23_4

  deallocate(c4)
  allocate(c4(c1%len),source=c1(1:3))
  if(any(c4 /= "xlf"))                                    error stop 24_4
  if(c4%len /= 3)                                         error stop 25_4
  if(ubound(c4,1) /= 7)                                   error stop 26_4

  deallocate(c4)
  allocate(c4(b2%l:b2%k),source=c1)

  if(lbound(c4,1) /= 1)                                   error stop 27_4
  if(ubound(c4,1) /= 0)                                   error stop 28_4

  allocate(base(2,c1%len) :: b3(c1%len))
  if(b3%l /= 7)                                           error stop 29_4
  if(ubound(b3,1) /= 7)                                   error stop 30_4

end


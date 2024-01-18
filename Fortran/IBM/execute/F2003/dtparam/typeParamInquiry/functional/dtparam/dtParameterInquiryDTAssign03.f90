!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDTAssign03.f
!*
!*  DATE                       : August 24 2008
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
!* 3. INTRINSIC ASSIGNMENT
!* 4. COMPONENT IS DERIVED TYPE
!* 5. DEFECT 355337
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(lb)
      integer,len :: lb
      character(lb) :: c1
      character(:),allocatable :: c2
      character(:),pointer     :: c3(:)
   end type
   type A(la)
      integer,len   :: la
      type(B(3))    :: b1
      type(B(2*la)) :: b2
   end type
end module

program dtParameterInquiryDTAssign03
  use m
  implicit none


  type(A(3)),target :: a1
  type(A(:)),allocatable :: a2
  type(A(:)),allocatable :: a3
  type(A(:)),pointer     :: a4
  type(A(:)),pointer     :: a5

  a1%b1%c1="xlftest"
  a1%b1%c2="xlftest"
  allocate(a1%b1%c3(a1%la),source=['123a','456b','789c'])

  a1%b2%c1="fortran"
  a1%b2%c2="xlftest team"
  allocate(a1%b2%c3(a1%la),source=['abc','def','ghi'])

  if(a1%la /= 3)                                           error stop 10_4
  associate(x=>a1%b1)
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)           error stop 11_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)           error stop 12_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 4)           error stop 13_4

    if(x%lb /= 3)                                          error stop 14_4
    if(x%c1 /= "xlf")                                      error stop 15_4
    if(x%c2 /= "xlftest")                                  error stop 16_4
    if(any(x%c3 /= ['123a','456b','789c']))                error stop 17_4
  end associate

  associate(x=>a1%b2)
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 6)           error stop 18_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 12)          error stop 19_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 3)           error stop 20_4

    if(x%lb /= 6)                                          error stop 21_4
    if(x%c1 /= "fortra")                                   error stop 22_4
    if(x%c2 /= "xlftest team")                             error stop 23_4
    if(any(x%c3 /= ['abc','def','ghi']))                   error stop 24_4
  end associate

  a2=a1

  if(a2%la /= 3)                                           error stop 30_4
  associate(x=>a2%b1)
    if(x%lb /= 3)                                          error stop 31_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)           error stop 32_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)           error stop 33_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 4)           error stop 34_4

    if(x%c1 /= "xlf")                                      error stop 35_4
    if(x%c2 /= "xlftest")                                  error stop 36_4
    if(any(x%c3 /= ['123a','456b','789c']))                error stop 37_4
  end associate

  associate(x=>a2%b2)
     !--- defect 355337--!
    if(x%lb /= 6)                                          error stop 38_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 6)           error stop 39_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 12)          error stop 40_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 3)           error stop 41_4
    if(x%c1 /= "fortra")                                   error stop 42_4
    if(x%c2 /= "xlftest team")                             error stop 43_4
    if(any(x%c3 /= ['abc','def','ghi']))                   error stop 44_4
  end associate

  a3=a2

  if(a3%la /= 3)                                           error stop 50_4
  associate(x=>a3%b1)
    if(x%lb /= 3)                                          error stop 51_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)           error stop 52_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)           error stop 53_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 4)           error stop 54_4

    if(x%c1 /= "xlf")                                      error stop 55_4
    if(x%c2 /= "xlftest")                                  error stop 56_4
    if(any(x%c3 /= ['123a','456b','789c']))                error stop 57_4
  end associate

  associate(x=>a3%b2)
     !--- defect 355337--!
     if(x%lb /= 6)                                         error stop 58_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 6)           error stop 59_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 12)          error stop 60_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 3)           error stop 61_4
    if(x%c1 /= "fortra")                                   error stop 62_4
    if(x%c2 /= "xlftest team")                             error stop 63_4
    if(any(x%c3 /= ['abc','def','ghi']))                   error stop 64_4
  end associate

   a4=>a1

  if(a4%la /= 3)                                           error stop 70_4
  associate(x=>a4%b1)
    if(x%lb /= 3)                                          error stop 71_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)           error stop 72_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)           error stop 73_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 4)           error stop 74_4

    if(x%c1 /= "xlf")                                      error stop 75_4
    if(x%c2 /= "xlftest")                                  error stop 76_4
    if(any(x%c3 /= ['123a','456b','789c']))                error stop 77_4
  end associate

  associate(x=>a4%b2)
     !--- defect 355337--!
    if(x%lb /= 6)                                          error stop 78_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 6)           error stop 79_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 12)          error stop 80_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 3)           error stop 81_4
    if(x%c1 /= "fortra")                                   error stop 82_4
    if(x%c2 /= "xlftest team")                             error stop 83_4
    if(any(x%c3 /= ['abc','def','ghi']))                   error stop 84_4
  end associate

  a5=>a4

  if(a5%la /= 3)                                           error stop 90_4
  associate(x=>a5%b1)
    if(x%lb /= 3)                                          error stop 91_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 3)           error stop 92_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 7)           error stop 93_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 4)           error stop 94_4

    if(x%c1 /= "xlf")                                      error stop 95_4
    if(x%c2 /= "xlftest")                                  error stop 96_4
    if(any(x%c3 /= ['123a','456b','789c']))                error stop 97_4
  end associate

  associate(x=>a5%b2)
     !--- defect 355337--!
    if(x%lb /= 6)                                          error stop 98_4
    if(x%c1%len /= len(x%c1) .or. x%c1%len /= 6)           error stop 99_4
    if(x%c2%len /= len(x%c2) .or. x%c2%len /= 12)          error stop 100_4
    if(x%c3%len /= len(x%c3) .or. x%c3%len /= 3)           error stop 101_4
    if(x%c1 /= "fortra")                                   error stop 102_4
    if(x%c2 /= "xlftest team")                             error stop 103_4
    if(any(x%c3 /= ['abc','def','ghi']))                   error stop 104_4
  end associate


end

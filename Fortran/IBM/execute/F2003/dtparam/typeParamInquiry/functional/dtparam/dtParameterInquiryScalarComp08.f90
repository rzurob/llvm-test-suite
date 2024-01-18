!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp08.f
!*
!*  DATE                       : July 8 2007
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
!* 4. ALLOCATABLE SCALAR CHARACTER COMPONENT
!* 5. DEFECT 353476
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,k3,k4,l1,l2,l3,l4)
     integer(1),kind           :: k1
     integer(2),kind           :: k2
     integer(int(4.1)),kind    :: k3
     integer(k2%kind * 4),kind :: k4

     integer(1),len         :: l1
     integer(2),len         :: l2
     integer(2*2),len       :: l3
     integer(k4%kind),len   :: l4

     character(kind=1,len=2),allocatable     :: c1
     character(len=:),allocatable            :: c2
     character(len=k1+k2+k3+k4),allocatable  :: c3
     character(len=l1),allocatable           :: c4
     character(len=k1%kind),allocatable      :: c5
     character(len=7*l1%kind),allocatable    :: c6
     character(2),allocatable :: c7
     character(2),allocatable :: c8
     character(len=l1+l2+l3+l4),allocatable  :: c9
     character(-8),allocatable               :: c10
     character(:),allocatable                :: c11

   end type
end module

  program dtParameterInquiryScalarComp08
  use m
  implicit none

  type(base(1,2,3,4,5,6,7,8))  :: t


  t%c1="xlftest"
  allocate(t%c2,source=t%c1)
  allocate(t%c3,source='')
  t%c4="fortran xlftest team"
  t%c5=t%c1//t%c2
  allocate(t%c6,source=1_"xlftest")
  t%c7=char(ichar('A'))//char(ichar('B'))
  t%c8=t%c4(1:2)
  allocate(t%c9,source=t%c8(1:0))
  t%c10="xlftest"(1:3)
  allocate(t%c11,source="hello"//" "//"world!")


  if(t%k1 /= 1)                                                error stop 10_4
  if(t%k2 /= 2)                                                error stop 11_4
  if(t%k3 /= 3)                                                error stop 12_4
  if(t%k4 /= 4)                                                error stop 13_4

  if(t%l1 /= 5)                                                error stop 14_4
  if(t%l2 /= 6)                                                error stop 15_4
  if(t%l3 /= 7)                                                error stop 16_4
  if(t%l4 /= 8)                                                error stop 17_4


  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)              error stop 18_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)              error stop 19_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /= 4)              error stop 20_4
  if(t%k4%kind /= kind(t%k4) .or. t%k4%kind /= 8)              error stop 21_4

  if(t%c1 /= "xl")                                             error stop 22_4
  if(t%c2 /= "xl")                                             error stop 23_4
  if(t%c3 /= '')                                               error stop 24_4
  if(t%c4 /= "fortr")                                          error stop 25_4
  if(t%c5 /= "x")                                              error stop 26_4
  if(t%c6 /= "xlftest")                                        error stop 27_4
  if(t%c7 /= 'AB')                                             error stop 28_4
  if(t%c8 /= "fo")                                             error stop 29_4
  if(t%c9 /= '')                                               error stop 30_4
  if(t%c10 /= '')                                              error stop 31_4
  if(t%c11 /= "hello world!")                                  error stop 32_4

  if(t%c1%kind /= kind(t%c1) .or. t%c1%kind /= 1)              error stop 33_4
  if(t%c2%kind /= kind(t%c2) .or. t%c2%kind /= 1)              error stop 34_4
  if(t%c3%kind /= kind(t%c3) .or. t%c3%kind /= 1)              error stop 35_4
  if(t%c4%kind /= kind(t%c4) .or. t%c4%kind /= 1)              error stop 36_4
  if(t%c5%kind /= kind(t%c5) .or. t%c5%kind /= 1)              error stop 37_4
  if(t%c6%kind /= kind(t%c6) .or. t%c6%kind /= 1)              error stop 38_4
  if(t%c7%kind /= kind(t%c7) .or. t%c7%kind /= 1)              error stop 39_4
  if(t%c8%kind /= kind(t%c8) .or. t%c8%kind /= 1)              error stop 40_4
  if(t%c9%kind /= kind(t%c9) .or. t%c9%kind /= 1)              error stop 41_4
  if(t%c10%kind /= kind(t%c10) .or. t%c10%kind /= 1)           error stop 42_4
  if(t%c11%kind /= kind(t%c11) .or. t%c11%kind /= 1)           error stop 43_4

  if(t%c1%len  /= len(t%c1) .or. t%c1%len /= 2)                error stop 44_4
  if(t%c2%len  /= len(t%c2) .or. t%c2%len /= 2)                error stop 45_4
  if(t%c3%len  /= len(t%c3) .or. t%c3%len /= 10)               error stop 46_4
  if(t%c4%len  /= len(t%c4) .or. t%c4%len /= 5)                error stop 47_4
  if(t%c5%len  /= len(t%c5) .or. t%c5%len /= 1)                error stop 48_4
  if(t%c6%len  /= len(t%c6) .or. t%c6%len /= 7)                error stop 49_4
  !--- defect 353476----!
  if(t%c7%len  /= len(t%c7) .or. t%c7%len /= 2)                error stop 50_4
  if(t%c8%len  /= len(t%c8) .or. t%c8%len /= 2)                error stop 51_4
  if(t%c9%len  /= len(t%c9) .or. t%c9%len /= 26)               error stop 52_4
  if(t%c10%len /= len(t%c10) .or. t%c10%len /= 0)              error stop 53_4
  if(t%c11%len /= len(t%c11) .or. t%c11%len /= 12)             error stop 54_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)              error stop 55_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)              error stop 56_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /= 4)              error stop 57_4
  if(t%l4%kind /= kind(t%l4) .or. t%l4%kind /= 8)              error stop 58_4


  end

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 9 2008
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
!* 4. CHARACTER SCALAR POINTER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   integer,parameter  :: k_2=2
   character(len=*),parameter  :: c_const="xlftest"

   type base(k1,k2,l1,l2)
       integer(k_2),kind :: k1
       integer(selected_int_kind(10)),kind :: k2

       integer(k1),len :: l1
       integer(k2),len :: l2

       character(len=:),pointer     :: c1=>null()
       character(len=:),pointer     :: c2=>null()
       character(kind=1,len=k1+k2),pointer       :: c3=>null()
       character(selected_int_kind(k1)),pointer  :: c4=>null()
       character(2**k_2),pointer    :: c5=>null()
       character(2*k2),pointer      :: c6=>null()
       character(len=len(1_"xlftest")),pointer   :: c7=>null()
       character(len(c_const)),pointer  :: c8=>null()
       character(l1%kind),pointer   :: c9=>null()
       character(3),pointer         :: c10=>null()
       character(len('')),pointer   :: c11=>null()
       character(-8),pointer        :: c12=>null()

   end type
end module

  program dtParameterInquiryScalarComp11
  use m
  implicit none

  type(base(2,4,5,10))  :: t

  character(len=5),target    :: ct="xlf"

  if(t%k1 /= 2)                                               error stop 10_4
  if(t%k2 /= 4)                                               error stop 11_4
  if(t%l1 /= 5)                                               error stop 12_4
  if(t%l2 /= 10)                                              error stop 13_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 2)             error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 8)             error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)             error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 4)             error stop 17_4

  if(t%c1%kind /= kind(t%c1) .or. t%c1%kind /= 1)             error stop 18_4
  if(t%c2%kind /= kind(t%c2) .or. t%c2%kind /= 1)             error stop 19_4
  if(t%c3%kind /= kind(t%c3) .or. t%c3%kind /= 1)             error stop 20_4
  if(t%c4%kind /= kind(t%c4) .or. t%c4%kind /= 1)             error stop 21_4
  if(t%c5%kind /= kind(t%c5) .or. t%c5%kind /= 1)             error stop 22_4
  if(t%c6%kind /= kind(t%c6) .or. t%c6%kind /= 1)             error stop 23_4
  if(t%c7%kind /= kind(t%c7) .or. t%c7%kind /= 1)             error stop 24_4
  if(t%c8%kind /= kind(t%c8) .or. t%c8%kind /= 1)             error stop 25_4
  if(t%c9%kind /= kind(t%c9) .or. t%c9%kind /= 1)             error stop 26_4
  if(t%c10%kind /= kind(t%c10) .or. t%c10%kind /= 1)          error stop 27_4
  if(t%c11%kind /= kind(t%c11) .or. t%c11%kind /= 1)          error stop 28_4
  if(t%c12%kind /= kind(t%c12) .or. t%c12%kind /= 1)          error stop 29_4

  t%c1=>ct
  t%c2=>t%c1(1:1)

  if(t%c1 /= "xlf")                                           error stop 30_4
  if(t%c2 /= "x")                                             error stop 31_4

  if(t%c1%len /= len(t%c1) .or. t%c1%len /= 5)                error stop 32_4
  if(t%c2%len /= len(t%c2) .or. t%c2%len /= 1)                error stop 33_4
  if(t%c3%len /= len(t%c3) .or. t%c3%len /= 6)                error stop 34_4

  if(t%c4%len /= len(t%c4) .or. t%c4%len /= 1)                error stop 35_4
  if(t%c5%len /= len(t%c5) .or. t%c5%len /= 4)                error stop 36_4
  if(t%c6%len /= len(t%c6) .or. t%c6%len /= 8)                error stop 37_4
  if(t%c7%len /= len(t%c7) .or. t%c7%len /= 7)                error stop 38_4
  if(t%c8%len /= len(t%c8) .or. t%c8%len /= 7)                error stop 39_4
  if(t%c9%len /= len(t%c9) .or. t%c9%len /= 2)                error stop 40_4
  if(t%c10%len /= len(t%c10) .or. t%c10%len /= 3)             error stop 41_4
  if(t%c11%len /= len(t%c11) .or. t%c11%len /= 0)             error stop 42_4
  if(t%c12%len /= len(t%c12) .or. t%c12%len /= 0)             error stop 43_4

  end

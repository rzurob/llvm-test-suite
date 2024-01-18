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
!* 4. SCALAR CHARACTER COMPONENT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
       integer(2),kind    :: k1
       integer(2*k1),kind :: k2

       integer(k1),len    :: l1
       integer(kind('a')),len  :: l2

       character(len=2)      :: c1="test c1"
       character(len=k1)     :: c2="test c2"
       character(len=2*(k2+k1))  :: c3="test c3"
       character(len=l1)     :: c4="test c4"
       character(len=k1+l1)  :: c5="test c5"
       character(-1)         :: c6="test c6"
       character(c1%len)     :: c7="test c7"
       character(k1%kind)    :: c8="test c8"
       character((l2%kind+l1%kind)*3)        :: c9="test c9"
       character(selected_char_kind("ascii")) :: c10="test c10"

   end type
end module

  program dtParameterInquiryScalarComp02
  use m
  implicit none

  type(base(1,1,1,1))   :: t1
  if(t1%k1 /=1 .or. t1%k2 /= 1 .or. t1%l1 /=1 .or. t1%l2 /= 1)  &
                                                       error stop 10_4

  if(t1%k1%kind /=kind(t1%k1) .or. t1%k1%kind /=2)     error stop 11_4
  if(t1%k2%kind /=kind(t1%k2) .or. t1%k2%kind /=2)     error stop 12_4
  if(t1%l1%kind /=kind(t1%l1) .or. t1%l1%kind /=1)     error stop 13_4
  if(t1%l2%kind /=kind(t1%l2) .or. t1%l2%kind /=1)     error stop 14_4

  if(t1%c1 /= "te" )                                   error stop 15_4
  if(t1%c1%len /= len(t1%c1) .or. t1%c1%len /=2)       error stop 16_4
  if(t1%c1%kind /= kind(t1%c1) .or. t1%c1%kind /=1)    error stop 17_4

  if(t1%c2 /="t")                                      error stop 18_4
  if(t1%c2%len /= len(t1%c2) .or. t1%c2%len /=1)       error stop 19_4
  if(t1%c2%kind /= kind(t1%c2) .or. t1%c2%kind /=1)    error stop 20_4

  if(t1%c3 /= "test")                                  error stop 21_4
  if(t1%c3%len /=len(t1%c3) .or. t1%c3%len /=4)        error stop 22_4
  if(t1%c3%kind /=kind(t1%c3) .or. t1%c3%kind /=1)     error stop 23_4

  if(t1%c4 /= "t")                                     error stop 24_4
  if(t1%c4%len /= len(t1%c4) .or. t1%c4%len /=1)       error stop 25_4
  if(t1%c4%kind /= kind(t1%c4) .or. t1%c4%kind /=1)    error stop 26_4

  if(t1%c5 /= "te")                                    error stop 27_4
  if(t1%c5%len /= len(t1%c5) .or. t1%c5%len /=2)       error stop 28_4
  if(t1%c5%kind /= kind(t1%c5) .or. t1%c5%kind /=1)    error stop 29_4


  if(t1%c6 /='')                                       error stop 30_4
  if(t1%c6%len /= len(t1%c6) .or. t1%c6%len /=0)       error stop 31_4
  if(t1%c6%kind /= kind(t1%c6) .or. t1%c6%kind /=1)    error stop 32_4

  if(t1%c7 /="te")                                     error stop 33_4
  if(t1%c7%len /= len(t1%c7) .or. t1%c7%len /=2)       error stop 34_4
  if(t1%c7%kind /= kind(t1%c7) .or. t1%c7%kind /=1)    error stop 35_4


  if(t1%c8 /="te")                                     error stop 36_4
  if(t1%c8%len /= len(t1%c8) .or. t1%c8%len /=2)       error stop 37_4
  if(t1%c8%kind /= kind(t1%c8) .or. t1%c8%kind /=1)    error stop 38_4

  if(t1%c9 /="test c")                                 error stop 39_4
  if(t1%c9%len /= len(t1%c9) .or. t1%c9%len /=6)       error stop 40_4
  if(t1%c9%kind /= kind(t1%c9) .or. t1%c9%kind /=1)    error stop 41_4

  if(t1%c10 /="t")                                     error stop 42_4
  if(t1%c10%len /= len(t1%c10) .or. t1%c10%len /=1)    error stop 43_4
  if(t1%c10%kind /= kind(t1%c10) .or. t1%c10%kind /=1) error stop 44_4


  end

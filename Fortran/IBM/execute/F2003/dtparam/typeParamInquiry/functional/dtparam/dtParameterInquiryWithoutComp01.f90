!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryWithoutComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 23 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY
!* 3. DIFFERENT DERIVED TYPE PARAMETER
!* 4. WITHOUT COMPONENT 
!* 5. DEFECT 353191
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type :: t(k1,k2,l1,l2)
        integer,kind :: k1
        integer(2),kind :: k2
        integer,len  :: l1
        integer(kind=8),len :: l2
   end type

end module

  program dtParameterInquiryWithoutComp01 
  use m
  implicit none

  integer,parameter   :: i1=2,i2=4
  character(len=*),parameter :: c1="xlftest"
   
  type( t(1,2,10,20) )         :: t1
  type( t(1_2,2_8,10_2,20_4) ) :: t2
  type( t(i1,i2,i1+i2,max(i1,i2)) ) :: t3
  type( t(selected_int_kind(10_8),kind(1_''),len(''),c1%len) ) :: t4
  type( t(k1=4,k2=8,l2=10_4,l1=i1*i2) )  :: t5
  type( t(int(0.000),2,-1,30) )  :: t6  
  type( t(0,0,0,0) )              :: t7
  type( t(1,2,-1,-2) )          :: t8  
  type( t(int(1.0),ichar('0'),int((3.0,4.0)),int(3.0/4.0)) )       :: t9
 
  if(t1%k1 /=1 .or. t1%k2 /=2 .or. t1%l1 /=10 .or. t1%l2 /=20) error stop 10_4
  if(t2%k1 /=1 .or. t2%k2 /=2 .or. t2%l1 /=10 .or. t2%l2 /=20) error stop 11_4
  if(t3%k1 /=2 .or. t3%k2 /=4 .or. t3%l1 /= 6 .or. t3%l2 /= 4) error stop 12_4
  if(t4%k1 /=8 .or. t4%k2 /=1 .or. t4%l1 /=0  .or. t4%l2 /= 7) error stop 13_4
  if(t5%k1 /=4 .or. t5%k2 /=8 .or. t5%l1 /=8 .or. t5%l2 /=10)  error stop 14_4
  if(t6%k1 /=0 .or. t6%k2 /=2 .or. t6%l1 /=-1 .or. t6%l2 /=30)  error stop 15_4
  if(t7%k1 /=0 .or. t7%k2 /=0 .or. t7%l1 /=0  .or. t7%l2 /=0)    error stop 16_4
  if(t8%k1 /=1 .or. t8%k2 /=2 .or. t8%l1 /=-1 .or. t8%l2 /=-2) error stop 17_4

  if(t9%k1 /=1 .or. t9%k2 /=48 .or. t9%l1 /=3 .or. t9%l2 /=0)    error stop 18_4

  if(t1%k1%kind /= kind(t1%k1)  .or. t1%k1%kind /=4 ) error stop 19_4
  if(t1%k2%kind /= kind(t1%k2)  .or. t1%k2%kind /=2 ) error stop 20_4
  if(t1%l1%kind /= kind(t1%l1)  .or. t1%l1%kind /=4 ) error stop 21_4 
  if(t1%l2%kind /= kind(t1%l2)  .or. t1%l2%kind /=8 ) error stop 22_4     

  if(t2%k1%kind /= kind(t2%k1)  .or. t2%k1%kind /=4 ) error stop 23_4
  if(t2%k2%kind /= kind(t2%k2)  .or. t2%k2%kind /=2 ) error stop 24_4
  if(t2%l1%kind /= kind(t2%l1)  .or. t2%l1%kind /=4 ) error stop 25_4
  if(t2%l2%kind /= kind(t2%l2)  .or. t2%l2%kind /=8 ) error stop 26_4  

  if(t3%k1%kind /= kind(t3%k1)  .or. t3%k1%kind /=4 ) error stop 27_4
  if(t3%k2%kind /= kind(t3%k2)  .or. t3%k2%kind /=2 ) error stop 28_4
  if(t3%l1%kind /= kind(t3%l1)  .or. t3%l1%kind /=4 ) error stop 29_4
  if(t3%l2%kind /= kind(t3%l2)  .or. t3%l2%kind /=8 ) error stop 30_4

  if(t4%k1%kind /= kind(t4%k1)  .or. t4%k1%kind /=4 ) error stop 31_4
  if(t4%k2%kind /= kind(t4%k2)  .or. t4%k2%kind /=2 ) error stop 32_4
  if(t4%l1%kind /= kind(t4%l1)  .or. t4%l1%kind /=4 ) error stop 33_4
  if(t4%l2%kind /= kind(t4%l2)  .or. t4%l2%kind /=8 ) error stop 34_4

  if(t5%k1%kind /= kind(t5%k1)  .or. t5%k1%kind /=4 ) error stop 35_4
  if(t5%k2%kind /= kind(t5%k2)  .or. t5%k2%kind /=2 ) error stop 36_4
  if(t5%l1%kind /= kind(t5%l1)  .or. t5%l1%kind /=4 ) error stop 37_4
  if(t5%l2%kind /= kind(t5%l2)  .or. t5%l2%kind /=8 ) error stop 38_4

  if(t6%k1%kind /= kind(t6%k1)  .or. t6%k1%kind /=4 ) error stop 39_4
  if(t6%k2%kind /= kind(t6%k2)  .or. t6%k2%kind /=2 ) error stop 40_4
  if(t6%l1%kind /= kind(t6%l1)  .or. t6%l1%kind /=4 ) error stop 41_4
  if(t6%l2%kind /= kind(t6%l2)  .or. t6%l2%kind /=8 ) error stop 42_4

  if(t7%k1%kind /= kind(t7%k1)  .or. t7%k1%kind /=4 ) error stop 43_4
  if(t7%k2%kind /= kind(t7%k2)  .or. t7%k2%kind /=2 ) error stop 44_4
  if(t7%l1%kind /= kind(t7%l1)  .or. t7%l1%kind /=4 ) error stop 45_4
  if(t7%l2%kind /= kind(t7%l2)  .or. t7%l2%kind /=8 ) error stop 46_4

  if(t8%k1%kind /= kind(t8%k1)  .or. t8%k1%kind /=4 ) error stop 47_4
  if(t8%k2%kind /= kind(t8%k2)  .or. t8%k2%kind /=2 ) error stop 48_4
  if(t8%l1%kind /= kind(t8%l1)  .or. t8%l1%kind /=4 ) error stop 49_4
  if(t8%l2%kind /= kind(t8%l2)  .or. t8%l2%kind /=8 ) error stop 50_4

  if(t9%k1%kind /= kind(t9%k1)  .or. t9%k1%kind /=4 ) error stop 51_4
  if(t9%k2%kind /= kind(t9%k2)  .or. t9%k2%kind /=2 ) error stop 52_4
  if(t9%l1%kind /= kind(t9%l1)  .or. t9%l1%kind /=4 ) error stop 53_4
  if(t9%l2%kind /= kind(t9%l2)  .or. t9%l2%kind /=8 ) error stop 54_4

end

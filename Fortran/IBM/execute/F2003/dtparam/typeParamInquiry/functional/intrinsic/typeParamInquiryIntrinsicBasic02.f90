!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicBasic02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!*  
!* 1. BASIC INQUIRY FOR INTRINSIC TYPE 
!* 2. TYPE PARAMETER INQUIRY FOR SCALAR     
!* 3. DIFFERENT KIND SELECTOR AND LENGTH SELECTOR 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicBasic02
    implicit none

    integer,parameter :: k1=1,k2=2,k4=4,k8=8,k16=16

    integer(kind=int(2.2)) :: i1
    integer(kind(4)) :: i2
    integer(selected_int_kind(10_8))   :: i3
    integer(1) :: i4

    real(kind=2*k4) :: r1
    real(kind(-1.0Q-10)) :: r2
    real(selected_real_kind(5,-10)) :: r3
    real(k16/k4)  :: r4

    complex(kind((4.0_8,5.0D10))) :: x1
    complex(kind=int(4.11))    :: x2
    complex(k16)  :: x3
    complex(kind=selected_real_kind(6,10)) :: x4

    logical(k1):: l1
    logical(kind(.true._4))  :: l2
    logical(k1+k1) :: l3
    logical(max(7,8))   :: l4

    character(len(k1_'HELLO'),kind(k1_'HELLO')) :: c1
    character(kind=1,len=2)  :: c2
    character(len=k4+k1,kind=selected_char_kind("ascii")) :: c3
    character(len('//'),kind('A'))     :: c4
    character(len(''))  :: c5
    character(kind('a'))   :: c6
    character(10,selected_char_kind("default")) :: c7
  
    if(i1%kind /= kind(i1) .or. i1%kind /= 2 .or. kind(i1) /= 2) stop 1
    if(i2%kind /= kind(i2) .or. i2%kind /= 4 .or. kind(i2) /= 4) stop 2
    if(i3%kind /= kind(i3) .or. i3%kind /= 8 .or. kind(i3) /= 8) stop 3
    if(i4%kind /= kind(i4) .or. i4%kind /= 1 .or. kind(i4) /= 1) stop 4

    if(r1%kind /= kind(r1) .or. r1%kind /= 8 .or. kind(r1) /= 8) stop 5 
    if(r2%kind /= kind(r2) .or. r2%kind /=16 .or. kind(r2) /=16) stop 6 
    if(r3%kind /= kind(r3) .or. r3%kind /= 4 .or. kind(r3) /= 4) stop 7 
    if(r4%kind /= kind(r4) .or. r4%kind /= 4 .or. kind(r4) /= 4) stop 8

    if(x1%kind /= kind(x1) .or. x1%kind /= 8 .or. kind(x1) /= 8) stop 9 
    if(x2%kind /= kind(x2) .or. x2%kind /= 4 .or. kind(x2) /= 4) stop 10 
    if(x3%kind /= kind(x3) .or. x3%kind /=16 .or. kind(x3) /=16) stop 11 
    if(x4%kind /= kind(x4) .or. x4%kind /= 4 .or. kind(x4) /= 4) stop 12     

    if(l1%kind /= kind(l1) .or. l1%kind /= 1 .or. kind(l1) /= 1) stop 13 
    if(l2%kind /= kind(l2) .or. l2%kind /= 4 .or. kind(l2) /= 4) stop 14
    if(l3%kind /= kind(l3) .or. l3%kind /= 2 .or. kind(l3) /= 2) stop 15
    if(l4%kind /= kind(l4) .or. l4%kind /= 8 .or. kind(l4) /= 8) stop 16

    if(c1%kind /= kind(c1) .or. c1%kind /= 1 .or. kind(c1) /= 1) stop 17
    if(c2%kind /= kind(c2) .or. c2%kind /= 1 .or. kind(c2) /= 1) stop 18
    if(c3%kind /= kind(c3) .or. c3%kind /= 1 .or. kind(c3) /= 1) stop 19
    if(c4%kind /= kind(c4) .or. c4%kind /= 1 .or. kind(c4) /= 1) stop 20 
    if(c5%kind /= kind(c5) .or. c5%kind /= 1 .or. kind(c5) /= 1) stop 21 
    if(c6%kind /= kind(c6) .or. c6%kind /= 1 .or. kind(c6) /= 1) stop 22
    if(c7%kind /= kind(c7) .or. c7%kind /= 1 .or. kind(c7) /= 1) stop 23

    if(c1%len  /= len(c1)  .or. c1%len  /= 5 .or. len(c1) /= 5)  stop 24 
    if(c2%len  /= len(c2)  .or. c2%len  /= 2 .or. len(c2) /= 2)  stop 25 
    if(c3%len  /= len(c3)  .or. c3%len  /= 5 .or. len(c3) /= 5)  stop 26 
    if(c4%len  /= len(c4)  .or. c4%len  /= 2 .or. len(c4) /= 2)  stop 27
    if(c5%len  /= len(c5)  .or. c5%len  /= 0 .or. len(c5) /= 0)  stop 28
    if(c6%len  /= len(c6)  .or. c6%len  /= 1 .or. len(c6) /= 1)  stop 29
    if(c7%len  /= len(c7)  .or. c7%len  /=10 .or. len(c7) /=10)  stop 30 


end


!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 7 2008
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
!* 3. TYPE PARAMETER INQUIRY FOR  NAMED CONSTANT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicBasic03
    implicit none

    integer,parameter :: k1=1,k2=2,k4=4,k8=8,k16=16

    integer(2),parameter       :: i1=-2_4
    integer,parameter          :: i2=1234
    integer(kind=1)            :: i3=-1.0

    real(8),parameter          :: r1=-1.0E-100_16
    real,parameter             :: r2=-1.0E-300_k16
    real(kind=16),parameter    :: r3=2_k4

    logical(2),parameter       :: l1=.false.
    logical,parameter          :: l2=.true._2
    logical(kind=8),parameter  :: l3=.false._k4

    complex(kind=8),parameter  :: x1=(-1.0D100,-1.0E10_8)
    complex,parameter          :: x2=(-1.0E15,1.0E30)
    complex(kind=16),parameter :: x3=(1.0E-100_k16,1.0E-50_16)

    character(kind=1,len=-7),parameter   :: c1='HELLO'
    character(2),parameter               :: c2=1_'HELLO'
    character(kind=1),parameter          :: c3=''
    character,parameter                  :: c4=''//''
    character(5,1),parameter             :: c5=char(5)

    character(len=*),parameter  :: c6="xlftest"
    character(len=*),parameter  :: c7=''
    character(len=*),parameter  :: c8=c1
    character(len=*),parameter  :: c9=c1//c2
    character(len=*),parameter  :: c10=char(ichar('a'))


    if(i1%kind /= kind(i1) .or. i1%kind /= 2 .or. kind(i1) /= 2) error stop 1
    if(i2%kind /= kind(i2) .or. i2%kind /= 4 .or. kind(i2) /= 4) error stop 2
    if(i3%kind /= kind(i3) .or. i3%kind /= 1 .or. kind(i3) /= 1) error stop 3

    if(r1%kind /= kind(r1) .or. r1%kind /= 8 .or. kind(r1) /= 8) error stop 4
    if(r2%kind /= kind(r2) .or. r2%kind /= 4 .or. kind(r2) /= 4) error stop 5
    if(r3%kind /= kind(r3) .or. r3%kind /=16 .or. kind(r3) /=16) error stop 6

    if(l1%kind /= kind(l1) .or. l1%kind /= 2 .or. kind(l1) /= 2) error stop 7
    if(l2%kind /= kind(l2) .or. l2%kind /= 4 .or. kind(l2) /= 4) error stop 8
    if(l3%kind /= kind(l3) .or. l3%kind /= 8 .or. kind(l3) /= 8) error stop 9

    if(x1%kind /= kind(x1) .or. x1%kind /= 8 .or. kind(x1) /= 8) error stop 10
    if(x2%kind /= kind(x2) .or. x2%kind /= 4 .or. kind(x2) /= 4) error stop 11
    if(l3%kind /= kind(l3) .or. l3%kind /= 8 .or. kind(l3) /= 8) error stop 12

    if(x1%kind /= kind(x1) .or. x1%kind /= 8 .or. kind(x1) /= 8) error stop 13
    if(x2%kind /= kind(x2) .or. x2%kind /= 4 .or. kind(x2) /= 4) error stop 14
    if(x3%kind /= kind(x3) .or. x3%kind /=16 .or. kind(x3) /=16) error stop 15

    if(c1%kind /= kind(c1) .or. c1%kind /= 1 .or. kind(c1) /= 1) error stop 16
    if(c2%kind /= kind(c2) .or. c2%kind /= 1 .or. kind(c2) /= 1) error stop 17
    if(c3%kind /= kind(c3) .or. c3%kind /= 1 .or. kind(c3) /= 1) error stop 18
    if(c4%kind /= kind(c4) .or. c4%kind /= 1 .or. kind(c4) /= 1) error stop 19
    if(c5%kind /= kind(c5) .or. c5%kind /= 1 .or. kind(c5) /= 1) error stop 20

    if(c1%len  /= len(c1)  .or. c1%len  /= 0 .or. len(c1)  /= 0) error stop 21
    if(c2%len  /= len(c2)  .or. c2%len  /= 2 .or. len(c2)  /= 2) error stop 22
    if(c3%len  /= len(c3)  .or. c3%len  /= 1 .or. len(c3)  /= 1) error stop 23
    if(c4%len  /= len(c4)  .or. c4%len  /= 1 .or. len(c4)  /= 1) error stop 24
    if(c5%len  /= len(c5)  .or. c5%len  /= 5 .or. len(c5)  /= 5) error stop 25

    if(c6%kind /= kind(c6) .or. c6%kind /= 1 .or. kind(c6) /= 1) error stop 26
    if(c7%kind /= kind(c7) .or. c7%kind /= 1 .or. kind(c7) /= 1) error stop 27
    if(c8%kind /= kind(c8) .or. c8%kind /= 1 .or. kind(c8) /= 1) error stop 28
    if(c9%kind /= kind(c9) .or. c9%kind /= 1 .or. kind(c9) /= 1) error stop 29
    if(c10%kind /= kind(c10) .or. c10%kind /= 1 .or. kind(c10) /= 1) error stop 30

    if(c6%len  /= len(c6)  .or. c6%len  /= 7 .or. len(c6)  /= 7) error stop 31
    if(c7%len  /= len(c7)  .or. c7%len  /= 0 .or. len(c7)  /= 0) error stop 32
    if(c8%len  /= len(c8)  .or. c8%len  /= 0 .or. len(c8)  /= 0) error stop 33
    if(c9%len  /= len(c9)  .or. c9%len  /= 2 .or. len(c9)  /= 2) error stop 34
    if(c10%len /= len(c10) .or. c10%len /= 1 .or. len(c10) /= 1) error stop 35


end


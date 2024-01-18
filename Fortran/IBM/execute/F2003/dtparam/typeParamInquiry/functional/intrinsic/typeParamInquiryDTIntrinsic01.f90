!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 6 2008 
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. INTRINSIC TYPE IN DT 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type t
      character :: c1
      character(len=10,kind=selected_char_kind("ascii")) :: c2
      character(len=4_2) ::c3
      character(len=len("xlftest")) :: c4
      character(len('xlf'//'test')) :: c5
      character(len(''))  :: c6
      character(-10,1)  :: c7
      
      real      :: r1
      real(selected_real_kind(5,300)) :: r2
      real(kind(-2.4Q100))  :: r3
      real(4_8)  :: r4
      real(2*2)  :: r5

      integer   :: i1
      integer(kind=selected_int_kind(10)) :: i2
      integer(kind=2)  :: i3
      integer(kind(12_8))  :: i4
      integer(kind(5555555)) :: i5
      integer(1+7) :: i6

      complex   :: x1
      complex(kind((2.D10,-2.D2))) :: x2
      complex(max(kind(4.5),kind(1111)))  :: x3
      complex(kind(8._16))  :: x4
      complex(2**4)  :: x5
        
      logical   :: l1
      logical(kind(.true.))  :: l2
      logical(kind(.false._8)) :: l3
      logical(kind=2)  :: l4
      logical(kind=kind(l2))  :: l5 
   
   end type

   contains

    subroutine checktype(tt)

      type(t) :: tt

     if(tt%c1%len /= len(tt%c1) .or. tt%c1%len /= 1) error stop 10_4
     if(tt%c1%kind /=kind(tt%c1) .or. tt%c1%kind /= 1) error stop 11_4

     if(tt%c2%len /= len(tt%c2) .or. tt%c2%len /= 10) error stop 12_4
     if(tt%c2%kind /=kind(tt%c2) .or. tt%c2%kind /= 1) error stop 13_4

     if(tt%c3%len /= len(tt%c3) .or. tt%c3%len /= 4) error stop 14_4
     if(tt%c3%kind /=kind(tt%c3) .or. tt%c3%kind /= 1) error stop 15_4

     if(tt%c4%len /= len(tt%c4) .or. tt%c4%len /= 7) error stop 16_4
     if(tt%c4%kind /=kind(tt%c4) .or. tt%c4%kind /= 1) error stop 17_4

     if(tt%c5%len /= len(tt%c5) .or. tt%c5%len /= 7) error stop 18_4
     if(tt%c5%kind /=kind(tt%c5) .or. tt%c5%kind /= 1) error stop 19_4

     if(tt%c6%len /= len(tt%c6) .or. tt%c6%len /= 0) error stop 20_4
     if(tt%c6%kind /=kind(tt%c6) .or. tt%c6%kind /= 1) error stop 21_4

     if(tt%c7%len /= len(tt%c7) .or. tt%c7%len /= 0) error stop 23_4
     if(tt%c7%kind /=kind(tt%c7) .or. tt%c7%kind /= 1) error stop 24_4

     if(tt%r1%kind /= kind(tt%r1) .or. tt%r1%kind /= 4) error stop 25_4
     if(tt%r2%kind /= kind(tt%r2) .or. tt%r2%kind /= 8) error stop 26_4
     if(tt%r3%kind /= kind(tt%r3) .or. tt%r3%kind /= 16) error stop 27_4
     if(tt%r4%kind /= kind(tt%r4) .or. tt%r4%kind /= 4) error stop 28_4
     if(tt%r5%kind /= kind(tt%r5) .or. tt%r5%kind /= 4) error stop 29_4
     if(tt%i1%kind /= kind(tt%i1) .or. tt%i1%kind /= 4) error stop 30_4
     if(tt%i2%kind /= kind(tt%i2) .or. tt%i2%kind /= 8) error stop 31_4
     if(tt%i3%kind /= kind(tt%i3) .or. tt%i3%kind /= 2) error stop 32_4
     if(tt%i4%kind /= kind(tt%i4) .or. tt%i4%kind /= 8) error stop 33_4
     if(tt%i5%kind /= kind(tt%i5) .or. tt%i5%kind /= 4) error stop 34_4
     if(tt%i6%kind /= kind(tt%i6) .or. tt%i6%kind /= 8) error stop 35_4
     if(tt%x1%kind /= kind(tt%x1) .or. tt%x1%kind /= 4) error stop 36_4
     if(tt%x2%kind /= kind(tt%x2) .or. tt%x2%kind /= 8) error stop 37_4
     if(tt%x3%kind /= kind(tt%x3) .or. tt%x3%kind /= 4) error stop 38_4
     if(tt%x4%kind /= kind(tt%x4) .or. tt%x4%kind /= 16) error stop 39_4
     if(tt%x5%kind /= kind(tt%x5) .or. tt%x5%kind /= 16) error stop 40_4

    end subroutine
end module

  program typeParamInquiryDTIntrinsic01
  use m
  implicit none

  integer :: i
  type(t) :: t1
  type(t),allocatable :: t2
  type(t),pointer  :: t3 
  type(t),target  :: t4
  type(t),allocatable :: t5(:)

  t3=>t4
  allocate(t5(2:3))

  call checktype(t1)
  call checktype(t2)
  call checktype(t3) 
  do i=lbound(t5,1),ubound(t5,1)
     call checktype(t5(i))
  enddo

end

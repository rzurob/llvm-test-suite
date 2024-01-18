!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp10.f
!*
!*  DATE                       : July 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER POINTER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   integer,parameter  :: k_4=4

   type base(k1,k2,k3,k4,l1,l2,l3,l4)
    integer(max(1,-1)),kind    :: k1
    integer(2_8),kind          :: k2
    integer(k_4),kind          :: k3
    integer(2*k_4),kind        :: k4

    integer(k1%kind),len       :: l1
    integer(k2),len            :: l2
    integer(k_4),len            :: l3
    integer(k4%kind),len       :: l4

    integer,pointer            :: i1
    integer(1),pointer         :: i2
    integer(2),pointer         :: i3
    integer(4),pointer         :: i4
    integer(8),pointer         :: i5
    integer(k3%kind),pointer   :: i6
    integer(k4),pointer        :: i7
    integer(k_4),pointer       :: i8
    integer(max(1,2)),pointer  :: i9
    integer(selected_int_kind(k1)),pointer  :: i10
    integer(kind=kind(10)),pointer          :: i11
    integer(kind(2)+kind(4)),pointer        :: i12

   end type
end module

  program dtParameterInquiryScalarComp10
  use m
  implicit none

  type(base(0,1,3,8,9,10,-1,-2))  :: t

  if(t%k1 /= 0)                                       error stop 10_4
  if(t%k2 /= 1)                                       error stop 11_4
  if(t%k3 /= 3)                                       error stop 12_4
  if(t%k4 /= 8)                                       error stop 13_4

  if(t%l1 /= 9)                                       error stop 14_4
  if(t%l2 /= 10)                                      error stop 15_4
  if(t%l3 /= -1)                                      error stop 16_4
  if(t%l4 /= -2)                                      error stop 17_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)     error stop 18_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)     error stop 19_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /= 4)     error stop 20_4
  if(t%k4%kind /= kind(t%k4) .or. t%k4%kind /= 8)     error stop 21_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)     error stop 22_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 1)     error stop 23_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /= 4)     error stop 24_4
  if(t%l4%kind /= kind(t%l4) .or. t%l4%kind /= 8)     error stop 25_4

  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 4)     error stop 26_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 1)     error stop 27_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 2)     error stop 28_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 4)     error stop 29_4

  if(t%i5%kind /= kind(t%i5) .or. t%i5%kind /= 8)     error stop 30_4
  if(t%i6%kind /= kind(t%i6) .or. t%i6%kind /= 4)     error stop 31_4
  if(t%i7%kind /= kind(t%i7) .or. t%i7%kind /= 8)     error stop 32_4
  if(t%i8%kind /= kind(t%i8) .or. t%i8%kind /= 4)     error stop 33_4
  if(t%i9%kind /= kind(t%i9) .or. t%i9%kind /= 2)     error stop 34_4
  if(t%i10%kind /= kind(t%i10) .or. t%i10%kind /= 1)  error stop 35_4
  if(t%i11%kind /= kind(t%i11) .or. t%i11%kind /= 4)  error stop 36_4
  if(t%i12%kind /= kind(t%i12) .or. t%i12%kind /= 8)  error stop 37_4

  end

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
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
!* 4. ALLOCATABLE INTEGER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   integer,parameter  :: i_const2=2

   type base(k1,k2,l1,l2)
     integer(1), kind :: k1
     integer(k1),kind :: k2
     integer(1),len       :: l1
     integer(k2),len      :: l2

     integer(k1),allocatable           :: i1
     integer(k1+k2),allocatable        :: i2
     integer(selected_int_kind(k1)),allocatable :: i3
     integer(max(k1,2*k2)),allocatable   :: i4
     integer(k1%kind),allocatable   :: i5
     integer(k2),allocatable        :: i6

     integer(1),allocatable         :: i7
     integer(i_const2),allocatable  :: i8
     integer(4_2),allocatable       :: i9
     integer(2*4_4),allocatable     :: i10

   end type
end module

  program dtParameterInquiryScalarComp07
  use m
  implicit none

  type(base(2,2,8,10))  :: t

  allocate(t%i1,source=t%k1+t%k2)
  t%i2=t%k1
  allocate(t%i3,source=t%k1)
  t%i4=max(t%k1%kind,t%k2%kind)
  t%i5=kind(t%k1)+t%k1%kind
  t%i6=t%i5

  if(t%k1 /= 2)                                            error stop 10_4
  if(t%k2 /= 2)                                            error stop 11_4
  if(t%l1 /= 8)                                            error stop 12_4
  if(t%l2 /=10)                                            error stop 13_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)          error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)          error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)          error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)          error stop 17_4

  if(t%i1 /= 4)                                            error stop 18_4
  if(t%i2 /= 2)                                            error stop 19_4
  if(t%i3 /= 2)                                            error stop 20_4
  if(t%i4 /= 2)                                            error stop 21_4
  if(t%i5 /= 2)                                            error stop 22_4
  if(t%i6 /= 2)                                            error stop 23_4

  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 2)          error stop 24_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 4)          error stop 25_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 1)          error stop 26_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 4)          error stop 27_4
  if(t%i5%kind /= kind(t%i5) .or. t%i5%kind /= 1)          error stop 28_4
  if(t%i6%kind /= kind(t%i6) .or. t%i6%kind /= 2)          error stop 29_4

  if(t%i7%kind /= kind(t%i7) .or. t%i7%kind /= 1)          error stop 30_4
  if(t%i8%kind /= kind(t%i8) .or. t%i8%kind /= 2)          error stop 31_4
  if(t%i9%kind /= kind(t%i9) .or. t%i9%kind /= 4)          error stop 32_4
  if(t%i10%kind /= kind(t%i10) .or. t%i10%kind /= 8)       error stop 33_4

  end

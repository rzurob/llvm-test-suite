!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp09.f
!*
!*  DATE                       : July 8 2008
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
!* 4. ALLOCATABLE SCALAR LOGICAL,COMPLEX,REAL COMPONENT
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

     logical(1),allocatable    :: a1
     logical(2),allocatable    :: a2
     logical(4),allocatable    :: a3
     logical(8),allocatable    :: a4
     logical(k1),allocatable   :: a5
     logical(2*k1%kind),allocatable :: a6
     logical(selected_int_kind(5)),allocatable :: a7
     logical(k2+k2),allocatable   :: a8
     logical(kind(.true.)),allocatable :: a9

     real,allocatable      :: r1
     real(4),allocatable   :: r2
     real(8),allocatable   :: r3
     real(16),allocatable  :: r4
     real(kind(1.2)),allocatable :: r5
     real(kind(4.0)),allocatable  :: r6
     real(k4%kind),allocatable  :: r7
     real(selected_real_kind(10,9)),allocatable :: r8
     real(k3%kind),allocatable                  :: r9
     real(kind(real(3.0,kind=8))),allocatable   ::r10

     complex,allocatable     :: x1
     complex(8),allocatable  :: x2
     complex(16),allocatable :: x3
     complex(selected_real_kind(9,10)),allocatable :: x4
     complex(kind((1.2e0,3.0e-1))),allocatable     :: x5
     complex(kind(cmplx(12,kind=8))),allocatable   :: x6

   end type
end module

  program dtParameterInquiryScalarComp09
  use m
  implicit none

  type(base(1,2,3,4,5,6,7,8))  :: t


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

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)              error stop 22_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)              error stop 23_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /= 4)              error stop 24_4
  if(t%l4%kind /= kind(t%l4) .or. t%l4%kind /= 8)              error stop 25_4

  if(t%a1%kind /= kind(t%a1) .or. t%a1%kind /= 1)              error stop 26_4
  if(t%a2%kind /= kind(t%a2) .or. t%a2%kind /= 2)              error stop 27_4
  if(t%a3%kind /= kind(t%a3) .or. t%a3%kind /= 4)              error stop 28_4
  if(t%a4%kind /= kind(t%a4) .or. t%a4%kind /= 8)              error stop 29_4
  if(t%a5%kind /= kind(t%a5) .or. t%a5%kind /= 1)              error stop 30_4
  if(t%a6%kind /= kind(t%a6) .or. t%a6%kind /= 2)              error stop 31_4
  if(t%a7%kind /= kind(t%a7) .or. t%a7%kind /= 4)              error stop 32_4
  if(t%a8%kind /= kind(t%a8) .or. t%a8%kind /= 4)              error stop 33_4
  if(t%a9%kind /= kind(t%a9) .or. t%a9%kind /= 4)              error stop 34_4

  if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /= 4)              error stop 35_4
  if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /= 4)              error stop 36_4
  if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /= 8)              error stop 37_4
  if(t%r4%kind /= kind(t%r4) .or. t%r4%kind /=16)              error stop 38_4
  if(t%r5%kind /= kind(t%r5) .or. t%r5%kind /= 4)              error stop 39_4
  if(t%r6%kind /= kind(t%r6) .or. t%r6%kind /= 4)              error stop 40_4
  if(t%r7%kind /= kind(t%r7) .or. t%r7%kind /= 8)              error stop 41_4
  if(t%r8%kind /= kind(t%r8) .or. t%r8%kind /= 8)              error stop 42_4
  if(t%r9%kind /= kind(t%r9) .or. t%r9%kind /= 4)              error stop 43_4
  if(t%r10%kind /= kind(t%r10) .or. t%r10%kind /= 8)           error stop 44_4

  if(t%x1%kind /= kind(t%x1) .or. t%x1%kind /= 4)              error stop 45_4
  if(t%x2%kind /= kind(t%x2) .or. t%x2%kind /= 8)              error stop 46_4
  if(t%x3%kind /= kind(t%x3) .or. t%x3%kind /=16)              error stop 47_4
  if(t%x4%kind /= kind(t%x4) .or. t%x4%kind /= 8)              error stop 48_4
  if(t%x5%kind /= kind(t%x5) .or. t%x5%kind /= 4)              error stop 49_4
  if(t%x6%kind /= kind(t%x6) .or. t%x6%kind /= 8)              error stop 50_4

  end

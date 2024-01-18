!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayIntegerComp05.f
!*
!*  DATE                       : July 15 2008 (edited on August 20, 2009)
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  track defect 353821
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER ALLOCATABLE ARRAY COMPONENT
!* 5. DEFECT 353689,353692 => 353821
!*
!*  CHANGES:
!*  The case tests the KIND of both type parameters and expressions involving them.
!*  As declared below, each kind and len parameter can have a kind different from
!*  the integer default.  "i7" and "i9" are declared as:
!*     integer(kind(k2)),allocatable :: i7(:)
!*     integer(kind(k1+k2)),allocatable :: i9(:)
!*  which means the KIND of k2 and the KIND of the sum of k1 and k2 are important.
!*  Unfortunately, there is a problem with the KIND of k2.  Here, kind(k2) and
!*  kind(k1+k2) should both be 2, and not the default kind.

!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind  :: k1
     integer(k1),kind :: k2
     integer(k1),len  :: l1
     integer(k1),len  :: l2

     integer(1),allocatable :: i1(:)
     integer(2),dimension(:),allocatable :: i2
     integer(4),allocatable :: i3(:,:)
     integer(8),dimension(:),allocatable :: i4

     integer(k1),dimension(:),allocatable :: i5
     integer(k1+k2),dimension(:,:),allocatable :: i6    ! defect 353689
     integer(kind(k2)),allocatable :: i7(:)             ! defect 353821
     integer(k1%kind),dimension(:,:),allocatable :: i8
     integer(kind(k1+k2)),allocatable :: i9(:)          ! defects 353692, 353821
   end type

end module

  program dtParameterInquiryArrayIntegerComp05
  use m
  implicit none

  type(base(2,2,4,8))  :: t


  integer,dimension(2:3,3:-1) :: a1=1
  allocate(t%i1(2))
  allocate(integer(2)::t%i2(3))
  t%i3=a1
  allocate(t%i4(5:4))

  allocate(t%i5(-3:3))
  allocate(integer(t%k1+t%k2) :: t%i6(-1:1,-1:0)) ! defect 353689
  allocate(t%i7(2))
  allocate(integer(t%k1%kind) ::t%i8(3:4,0:-1))
  allocate(integer(kind(t%k1+t%k2)) :: t%i9(4))


  if(t%k1 /= 2)                                           error stop 10_4
  if(t%k2 /= 2)                                           error stop 11_4
  if(t%l1 /= 4)                                           error stop 12_4
  if(t%l2 /= 8)                                           error stop 13_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)         error stop 14_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)         error stop 15_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)         error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)         error stop 17_4

  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 1)         error stop 18_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /= 2)         error stop 19_4
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /= 4)         error stop 20_4
  if(t%i4%kind /= kind(t%i4) .or. t%i4%kind /= 8)         error stop 21_4
  if(t%i5%kind /= kind(t%i5) .or. t%i5%kind /= 2)         error stop 22_4
  if(t%i6%kind /= kind(t%i6) .or. t%i6%kind /= 4)         error stop 23_4
  if(t%i7%kind /= kind(t%i7) .or. t%i7%kind /= 2)         error stop 24_4
  if(t%i8%kind /= kind(t%i8) .or. t%i8%kind /= 1)         error stop 25_4
  if(t%i9%kind /= kind(t%i9) .or. t%i9%kind /= 2)         error stop 26_4

  if(lbound(t%i1,1) /=1 .or. ubound(t%i1,1) /= 2)         error stop 27_4
  if(lbound(t%i2,1) /=1 .or. ubound(t%i2,1) /= 3)         error stop 28_4
  if(lbound(t%i3,1) /=2 .or. ubound(t%i3,1) /= 3)         error stop 29_4
  if(lbound(t%i3,2) /=1 .or. ubound(t%i3,2) /= 0)         error stop 30_4
  if(lbound(t%i4,1) /=1 .or. ubound(t%i4,1) /= 0)         error stop 31_4
  if(lbound(t%i5,1) /=-3 .or. ubound(t%i5,1) /=3)         error stop 32_4

  end

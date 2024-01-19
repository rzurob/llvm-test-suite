!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 12 2008
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
!* 4. ALLOCATABLE REAL,COMPLEX ARRAY COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind :: k1=2
     integer(2),kind :: k2=4
     integer(2),len  :: l1=k1+k2
     integer(4),len  :: l2=k2-k1

     real(4),dimension(:,:),pointer :: r1
     real(8),pointer    :: r2(:,:)
     real(16),pointer   :: r3(:)
     real(2*k1),pointer :: r4(:)
     real(k2),pointer   :: r5(:)
     real(4),pointer    :: r6(:)
     real(4*k2%kind),pointer  :: r7(:)
     real(k1*2+k2),pointer    :: r8(:)
     real(kind(k1+k2)*4),pointer :: r9(:)
     real(kind(111.11)),pointer  :: r10(:)

     complex(4),pointer  :: x1(:)
     complex(8),pointer  :: x2(:)
     complex(16),pointer :: x3(:)
     complex(2*k1),pointer :: x4(:)
     complex(k2),pointer :: x5(:)
     complex(k2),pointer :: x6(:)
     complex(k1*k2),pointer   :: x7(:)
     complex(4*k2%kind),pointer  ::x8(:)
     complex(8*kind(k1)),pointer :: x9(:)
     complex(2*k1+k2),pointer    :: x10(:)

   end type

end module

  program dtParameterInquiryArrayRealComplexComp03
  use m
  implicit none

  type(base)  :: t


  if(t%k1 /= 2)                                             error stop 10_4
  if(t%k2 /= 4)                                             error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)           error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)           error stop 13_4

  if(t%l1 /=6)                                              error stop 14_4
  if(t%l2 /=2)                                              error stop 15_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)           error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 4)           error stop 17_4

  if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /= 4)           error stop 18_4
  if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /= 8)           error stop 19_4
  if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /=16)           error stop 20_4
  if(t%r4%kind /= kind(t%r4) .or. t%r4%kind /= 4)           error stop 21_4
  if(t%r5%kind /= kind(t%r5) .or. t%r5%kind /= 4)           error stop 22_4
  if(t%r6%kind /= kind(t%r6) .or. t%r6%kind /= 4)           error stop 23_4
  if(t%r7%kind /= kind(t%r7) .or. t%r7%kind /= 8)           error stop 24_4
  if(t%r8%kind /= kind(t%r8) .or. t%r8%kind /= 8)           error stop 25_4
  if(t%r9%kind /= kind(t%r9) .or. t%r9%kind /= 8)           error stop 26_4
  if(t%r10%kind /= kind(t%r10) .or. t%r10%kind /= 4)        error stop 27_4

  if(t%x1%kind /= kind(t%x1) .or. t%x1%kind /= 4)           error stop 28_4
  if(t%x2%kind /= kind(t%x2) .or. t%x2%kind /= 8)           error stop 29_4
  if(t%x3%kind /= kind(t%x3) .or. t%x3%kind /=16)           error stop 30_4
  if(t%x4%kind /= kind(t%x4) .or. t%x4%kind /= 4)           error stop 31_4
  if(t%x5%kind /= kind(t%x5) .or. t%x5%kind /= 4)           error stop 32_4
  if(t%x6%kind /= kind(t%x6) .or. t%x6%kind /= 4)           error stop 33_4
  if(t%x7%kind /= kind(t%x7) .or. t%x7%kind /= 8)           error stop 34_4
  if(t%x8%kind /= kind(t%x8) .or. t%x8%kind /= 8)           error stop 35_4
  if(t%x9%kind /= kind(t%x9) .or. t%x9%kind /= 8)           error stop 36_4
  if(t%x10%kind /= kind(t%x10) .or. t%x10%kind /= 8)        error stop 37_4


  end

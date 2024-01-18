!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp12.f
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. DIFFERENT TYPE PARAMETER
!* 4. LOGICAL,REAL,COMPLEX SCALAR POINTER COMPONENT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

   integer,parameter  :: k8=8
   type base(k)
     integer,kind  :: k

     logical,pointer    :: l1
     logical(1),pointer :: l2
     logical(2),pointer :: l3
     logical(4),pointer :: l4
     logical(8),pointer :: l5
     logical(k),pointer :: l6
     logical(kind(k)+k%kind),pointer :: l7
     logical(k+k),pointer :: l8
     logical(int(k)),pointer :: l9
     logical(selected_int_kind(k%kind)),pointer :: l10

     real,pointer       :: r1
     real(4),pointer    :: r2
     real(8),pointer    :: r3
     real(16),pointer   :: r4
     real(k+k),pointer    :: r5
     real(2**k),pointer :: r6
     real(k8),pointer   :: r7
     real(kind(5)),pointer :: r8

     complex,pointer    :: x1
     complex(4),pointer :: x2
     complex(8),pointer :: x3
     complex(16),pointer :: x4
     complex(max(1,2*k)),pointer  :: x5
     complex(selected_int_kind(9)),pointer :: x6

   end type
end module

  program dtParameterInquiryScalarComp12
  use m
  implicit none

  type(base(2))  :: t

  if(t%k /= 2)                                           error stop 10_4
  if(t%k%kind /= kind(t%k) .or. t%k%kind /= 4)           error stop 11_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 4)        error stop 12_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 1)        error stop 13_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /= 2)        error stop 14_4
  if(t%l4%kind /= kind(t%l4) .or. t%l4%kind /= 4)        error stop 15_4
  if(t%l5%kind /= kind(t%l5) .or. t%l5%kind /= 8)        error stop 16_4
  if(t%l6%kind /= kind(t%l6) .or. t%l6%kind /= 2)        error stop 17_4
  if(t%l7%kind /= kind(t%l7) .or. t%l7%kind /= 8)        error stop 18_4
  if(t%l8%kind /= kind(t%l8) .or. t%l8%kind /= 4)        error stop 19_4
  if(t%l9%kind /= kind(t%l9) .or. t%l9%kind /= 2)        error stop 20_4
  if(t%l10%kind /= kind(t%l10) .or. t%l10%kind /= 2)     error stop 21_4

  if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /= 4)        error stop 22_4
  if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /= 4)        error stop 23_4
  if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /= 8)        error stop 24_4
  if(t%r4%kind /= kind(t%r4) .or. t%r4%kind /=16)        error stop 25_4
  if(t%r5%kind /= kind(t%r5) .or. t%r5%kind /= 4)        error stop 26_4
  if(t%r6%kind /= kind(t%r6) .or. t%r6%kind /= 4)        error stop 27_4
  if(t%r7%kind /= kind(t%r7) .or. t%r7%kind /= 8)        error stop 28_4
  if(t%r8%kind /= kind(t%r8) .or. t%r8%kind /= 4)        error stop 29_4

  if(t%x1%kind /= kind(t%x1) .or. t%x1%kind /= 4)        error stop 30_4
  if(t%x2%kind /= kind(t%x2) .or. t%x2%kind /= 4)        error stop 31_4
  if(t%x3%kind /= kind(t%x3) .or. t%x3%kind /= 8)        error stop 32_4
  if(t%x4%kind /= kind(t%x4) .or. t%x4%kind /=16)        error stop 33_4
  if(t%x5%kind /= kind(t%x5) .or. t%x5%kind /= 4)        error stop 34_4
  if(t%x6%kind /= kind(t%x6) .or. t%x6%kind /= 4)        error stop 35_4

  end

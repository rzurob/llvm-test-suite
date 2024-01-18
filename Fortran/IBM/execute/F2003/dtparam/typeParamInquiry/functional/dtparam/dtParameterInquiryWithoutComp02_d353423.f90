!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryWithoutComp02_d353423.f
!*
!*  DATE                       : July 13 2008
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
!* 4. WITHOUT COMPONENT
!* 5. DEFECT 353423
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type :: base1(k1,k2,k3,l1,l2,l3,l4)
        integer,kind :: k1
        integer,kind :: k2
        integer(k1+k2),kind :: k3
        integer(2),len  :: l1
        integer(2+0),len :: l2
        integer(selected_int_kind(k3%kind)),len :: l3
        integer(k3%kind),len :: l4
   end type

   type base2(k1,k2,k3,l1,l2)
      integer(1),kind  :: k1
      integer(k1),kind  :: k2
      integer(k1+k2),kind :: k3
      integer(k3%kind),len :: l1
      integer(kind(k3)),len :: l2
      integer(k2%kind),allocatable :: i1(:)
   end type
end module

  program dtParameterInquiryWithoutComp02_d353423
  use m
  implicit none

  type(base1(1,1,1,1,1,1,1)) :: t1
  type(base2(2,2,1,1,1))      :: t2

  if(t1%k1 /= 1 .or. t1%k2 /= 1 .or. t1%k3 /= 1)     error stop 10_4
  if(t1%l1 /= 1 .or. t1%l2 /= 1 .or. t1%l3 /= 1)     error stop 11_4
  if(t1%k1%kind /= kind(t1%k1) .or. t1%k1%kind /= 4) error stop 12_4
  if(t1%k2%kind /= kind(t1%k2) .or. t1%k2%kind /= 4) error stop 13_4
  if(t1%k3%kind /= kind(t1%k3) .or. t1%k3%kind /= 2) error stop 14_4
  if(t1%l1%kind /= kind(t1%l1) .or. t1%l1%kind /= 2) error stop 15_4
  if(t1%l2%kind /= kind(t1%l2) .or. t1%l2%kind /= 2) error stop 16_4
  if(t1%l3%kind /= kind(t1%l3) .or. t1%l3%kind /= 1) error stop 17_4
  !-- defect 353423---!
  if(t1%l4%kind /= kind(t1%l4) .or. t1%l4%kind /= 2) error stop 18_4

  if(t2%k1 /= 2)                                       error stop 20_4
  if(t2%k1%kind /= kind(t2%k1) .or. t2%k1%kind /= 1)   error stop 21_4
  if(t2%k2 /= 2)                                       error stop 22_4
  if(t2%k2%kind /= kind(t2%k2) .or. t2%k2%kind /= 2)   error stop 23_4
  if(t2%k3 /= 1)                                       error stop 24_4
  if(t2%k3%kind /= kind(t2%k3) .or. t2%k3%kind /= 4)   error stop 25_4
  if(t2%l1 /= 1)                                       error stop 26_4
  if(t2%l1%kind /= kind(t2%l1) .or. t2%l1%kind /= 4)   error stop 27_4
  if(t2%l2 /= 1)                                       error stop 28_4
  if(t2%l2%kind /= kind(t2%l2) .or. t2%l2%kind /= 4)   error stop 29_4
  if(t2%i1%kind /= kind(t2%i1) .or. t2%i1%kind /= 2)   error stop 30_4

  end

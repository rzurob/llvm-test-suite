!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp06.f
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
!* 4. SCALAR COMPLEX COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890

module m

   type base(k1,k2,k3,k4,l1,l2,l3)
      integer,kind          :: k1
      integer(2),kind       :: k2
      integer(k1),kind      :: k3
      integer(k2+k2),kind   :: k4

      integer,len           :: l1
      integer(k1),len       :: l2
      integer(k2+k2),len    :: l3

      complex(kind(4.0))    :: x1
      complex(kind(real(k1))) :: x2
      complex(selected_real_kind(6,10)) :: x3
      complex               :: x4

   end type
end module

  program dtParameterInquiryScalarComp06
  use m
  implicit none
  type(base(2,4,4,1,-4,-8_4,2_2)) :: t

  if(t%k1%kind /= kind(t%k1)  .or. t%k1%kind /= 4)       error stop 10_4
  if(t%k2%kind /= kind(t%k2)  .or. t%k2%kind /= 2)       error stop 11_4
  if(t%k3%kind /= kind(t%k3)  .or. t%k3%kind /= 2)       error stop 12_4
  if(t%k4%kind /= kind(t%k4)  .or. t%k4%kind /= 8)       error stop 13_4

  if(t%l1%kind /= kind(t%l1)  .or. t%l1%kind /= 4)       error stop 14_4
  if(t%l2%kind /= kind(t%l2)  .or. t%l2%kind /= 2)       error stop 15_4
  if(t%l3%kind /= kind(t%l3)  .or. t%l3%kind /= 8)       error stop 16_4

  if(t%x1%kind /= kind(t%x1)  .or. t%x1%kind /= 4)       error stop 17_4
  if(t%x2%kind /= kind(t%x2)  .or. t%x2%kind /= 4)       error stop 18_4
  if(t%x3%kind /= kind(t%x3)  .or. t%x3%kind /= 4)       error stop 19_4
  if(t%x4%kind /= kind(t%x4)  .or. t%x4%kind /= 4)       error stop 20_4

  end

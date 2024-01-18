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
!* 4. WITHOUT COMPONENT,DEFAULT INTIALIZATION
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   integer,parameter :: k=5

   type base(k1,k2,k3,k4,l1,l2,l3,l4)
     integer(1),kind :: k1=k
     integer(selected_int_kind(4)),kind :: k2=selected_int_kind(4)
     integer(int(2.2)+max(1,2)),kind :: k3=int(2.2)+max(1,2)
     integer(8),kind :: k4=kind('a')

     integer(1),len  :: l1=kind(k1+k2)
     integer(selected_int_kind(4)),len :: l2=selected_int_kind(4)
     integer(k+3),len       :: l3=k+3
     integer(kind(k+3)),len :: l4=kind(k+3)
   end type
end module
  program dtParameterInquiryWithoutComp03
  use m
  implicit none

  type(base)  :: t

  if(t%k1 /= 5)                                   error stop 10_4
  if(t%k2 /= 2)                                   error stop 11_4
  if(t%k3 /= 4)                                   error stop 12_4
  if(t%k4 /= 1)                                   error stop 13_4

  if(t%l1 /= 2)                                   error stop 14_4
  if(t%l2 /= 2)                                   error stop 15_4
  if(t%l3 /= 8)                                   error stop 16_4
  if(t%l4 /= 4)                                   error stop 17_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /=1)  error stop 18_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /=2)  error stop 19_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /=4)  error stop 20_4
  if(t%k4%kind /= kind(t%k4) .or. t%k4%kind /=8)  error stop 21_4

  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /=1)  error stop 22_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /=2)  error stop 23_4
  if(t%l3%kind /= kind(t%l3) .or. t%l3%kind /=8)  error stop 24_4
  if(t%l4%kind /= kind(t%l4) .or. t%l4%kind /=4)  error stop 25_4


  end

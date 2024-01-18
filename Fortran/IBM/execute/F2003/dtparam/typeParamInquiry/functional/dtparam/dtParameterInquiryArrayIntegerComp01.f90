!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayIntegerComp01.f
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
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER ARRAY COMPONENT
!* 5. DEFECT 353331
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,k3,l1,l2,l3)

     integer(1),kind  :: k1=2
     integer(k1),kind :: k2=k1
     integer(k1+k2),kind :: k3=kind('a')

     integer(1),len  :: l1=max(1,2)
     integer(k1),len :: l2=k2
     integer(k2),len :: l3=int(k1+k2)

     integer(1),dimension(3) :: i1=[-1,0,-2]
     integer(k1),dimension(l1) :: i2=0
     integer(k1+k2) :: i3(l1,l2)=k2+k2
     integer(2) :: i4(l1:l2)=k1%kind

     integer(max(2,4)) :: i5(lbound(i1,1)+2 : ubound(i1,1)+2)
     integer(4) :: i6(l1:l1+l2) ! defect 353331

   end type

end module

  program dtParameterInquiryArrayIntegerComp01
  use m
  implicit none

  type(base)  :: t


  if(t%k1 /= 2)                                         error stop 10_4
  if(t%k2 /= 2)                                         error stop 11_4
  if(t%k3 /= 1)                                         error stop 12_4

  if(t%l1 /= 2)                                         error stop 13_4
  if(t%l2 /= 2)                                         error stop 14_4
  if(t%l3 /= 4)                                         error stop 15_4

  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 1)       error stop 16_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)       error stop 17_4
  if(t%k3%kind /= kind(t%k3) .or. t%k3%kind /= 4)       error stop 18_4

  if(t%l1%kind /=kind(t%l1) .or. t%l1%kind /= 1)        error stop 19_4
  if(t%l2%kind /=kind(t%l2) .or. t%l2%kind /= 2)        error stop 20_4
  if(t%l3%kind /=kind(t%l3) .or. t%l3%kind /= 2)        error stop 21_4

  if(any(t%i1 /= [-1,0,-2]))                            error stop 22_4
  if(any(t%i2 /= 0))                                    error stop 23_4
  if(any(t%i3 /= 2*t%k2))                               error stop 24_4
  if(any(t%i4 /= t%k1%kind))                            error stop 25_4

  if(t%i1%kind /=kind(t%i1) .or. t%i1%kind /= 1)        error stop 26_4
  if(t%i2%kind /=kind(t%i2) .or. t%i2%kind /= 2)        error stop 27_4
  if(t%i3%kind /=kind(t%i3) .or. t%i3%kind /= 4)        error stop 28_4
  if(t%i4%kind /=kind(t%i4) .or. t%i4%kind /= 2)        error stop 29_4
  if(t%i5%kind /=kind(t%i5) .or. t%i5%kind /= 4)        error stop 30_4
  if(t%i6%kind /=kind(t%i6) .or. t%i6%kind /= 4)        error stop 31_4


  end

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam01.f
!*
!*  DATE                       : July 17 2008
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
!* 3. WITHOUT COMPONENT
!* 4. ASSUMED LENGTH DUMMY ARGUEMENT
!* 5. DEFECT 353943
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type ::base(k1,k2,l1,l2)
      integer(2),kind :: k1
      integer(4),kind :: k2
      integer(2),len  :: l1
      integer(4),len  :: l2
   end type

end module

  program dtParameterInquiryAssumedTypeParam01
  use m
  implicit none

  type(base(7,8,4,5)) :: b
    if(b%k1 /= 7)                                          error stop 10_4
    if(b%k2 /= 8)                                          error stop 11_4
    if(b%l1 /= 4)                                          error stop 12_4
    if(b%l2 /= 5)                                          error stop 13_4

    if(b%k1%kind /= kind(b%k1) .or. b%k1%kind /= 2)       error stop 14_4
    if(b%k2%kind /= kind(b%k2) .or. b%k2%kind /= 4)       error stop 15_4
    if(b%l1%kind /= kind(b%l1) .or. b%l1%kind /= 2)       error stop 16_4
    if(b%l2%kind /= kind(b%l2) .or. b%l2%kind /= 4)       error stop 17_4

    call sub1(b)
  contains
    subroutine sub1(c)
    type(base(7,8,l1=*,l2=*)) :: c

    if(c%k1 /= 7)                                          error stop 18_4
    if(c%k2 /= 8)                                          error stop 19_4
    if(c%l1 /= 4)                                          error stop 20_4
    if(c%l2 /= 5)                                          error stop 21_4

    if(c%k1%kind /= kind(c%k1) .or. c%k1%kind /= 2)       error stop 22_4
    if(c%k2%kind /= kind(c%k2) .or. c%k2%kind /= 4)       error stop 23_4
    if(c%l1%kind /= kind(c%l1) .or. c%l1%kind /= 2)       error stop 24_4
    if(c%l2%kind /= kind(c%l2) .or. c%l2%kind /= 4)       error stop 25_4
    end subroutine
end

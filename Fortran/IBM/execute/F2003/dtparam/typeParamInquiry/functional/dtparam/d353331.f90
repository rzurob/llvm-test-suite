!*  ===================================================================
!*
!*  DATE                       : July 3 2008
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
!* 2. DEFECT 353331
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base1(l1,l2)
       integer(1),len  :: l1
       integer(2),len  :: l2
       character(len=l1+l2)  :: c1
       character(len=l1*l2)  :: c2
   end type
   type base2(k1,l1,l2)
     integer(1),kind  :: k1=2
     integer(1),len   :: l1=2
     integer(k1),len  :: l2=2
     integer          :: i(l1:l1+l2)
   end type

end module

program d353331
  use m
  implicit none
  type(base1(4,10)) :: b1
  type(base2)       :: b2

  if(b1%l1 /= 4)                                      error stop 10_4
  if(b1%l2 /= 10)                                     error stop 11_4
  if(b1%l1%kind /= kind(b1%l1) .or. b1%l1%kind /= 1)  error stop 12_4
  if(b1%l2%kind /= kind(b1%l2) .or. b1%l2%kind /= 2)  error stop 13_4
  if(b1%c1%len /= len(b1%c1) .or. b1%c1%len /= 14)    error stop 14_4
  if(b1%c2%len /= len(b1%c2) .or. b1%c2%len /= 40)    error stop 15_4

  if(b2%k1 /= 2)                                      error stop 16_4
  if(b2%l1 /= 2)                                      error stop 17_4
  if(b2%l2 /= 2)                                      error stop 18_4
  if(b2%k1%kind /= kind(b2%k1) .or. b2%k1%kind /= 1)  error stop 19_4
  if(b2%l1%kind /= kind(b2%l1) .or. b2%l1%kind /= 1)  error stop 20_4
  if(b2%l2%kind /= kind(b2%l2) .or. b2%l2%kind /= 2)  error stop 21_4
  if(lbound(b2%i,1) /= 2)                             error stop 22_4
  if(ubound(b2%i,1) /= 4)                             error stop 23_4


end

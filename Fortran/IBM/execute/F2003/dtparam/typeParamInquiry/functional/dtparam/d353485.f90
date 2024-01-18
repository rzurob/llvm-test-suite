!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353485.f
!*
!*  DATE                       : Sept 08 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353485
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k)
       integer,kind   :: k
       real :: r1
       real(kind(r1))  :: r2
       real(kind(r1)) :: r3
   end type
end module

program d353485
   use m
   implicit none
   type(base(2)) :: t

   if(t%k /= 2)                                     error stop 10_4
   if(t%k%kind /= kind(t%k) .or. t%k%kind /= 4)     error stop 11_4
   if(t%r1%kind /= kind(t%r1) .or. t%r1%kind /= 4)  error stop 12_4
   if(t%r2%kind /= kind(t%r2) .or. t%r2%kind /= 4)  error stop 13_4
   if(t%r3%kind /= kind(t%r3) .or. t%r3%kind /= 4)  error stop 14_4

end


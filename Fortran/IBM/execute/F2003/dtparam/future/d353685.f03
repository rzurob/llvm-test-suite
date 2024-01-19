!*  ===================================================================
!*
!*  DATE                       : July 14 2008
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
!* 2. DEFECT 353685
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
     integer(1),kind  :: k=2
     integer(k),len   :: l=k
     integer(2)       :: i1(kind(l)+kind(l))
     integer(8),dimension(kind(k+k)+k%kind) :: i2=kind(k+k)+k%kind
   end type
end module

program d353685
  use m
  implicit none
  type(base)  :: t

  if(t%k /= 2)                                        error stop 10_4
  if(t%l /= 2)                                        error stop 11_4
  if(t%k%kind /= kind(t%k) .or. t%k%kind /= 1)        error stop 12_4
  if(t%l%kind /= kind(t%l) .or. t%l%kind /= 2)        error stop 13_4
  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /= 2)     error stop 14_4
  if(ubound(t%i1,1) /= 4)                             error stop 15_4
  if(ubound(t%i2,1) /= 2)                             error stop 16_4
  if(any(t%i2 /= 2))                                  error stop 17_4

end


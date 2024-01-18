!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355810.f
!*
!*  DATE                       : September 07 2008
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
!* 2. DEFECT d355810
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,k2)
      integer(1),kind    :: k1
      integer(2*k1),kind :: k2
      real(4+kind(k2))  :: r5
   end type
end module

program d355810
  use m
  implicit none

  type(base(2,2))  :: t
  print *,t%k1,t%k1%kind,kind(t%k1)
  print *,t%k2,t%k2%kind,kind(t%k2)
  print *,t%r5%kind,kind(t%r5)

end


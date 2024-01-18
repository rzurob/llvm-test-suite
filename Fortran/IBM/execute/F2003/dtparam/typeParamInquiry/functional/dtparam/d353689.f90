!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353689.f
!*
!*  DATE                       : July 15 2008
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
!* 2. DEFECT 353689
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,k2)
      integer(2),kind :: k1=2
      integer(2),kind :: k2=2
      integer(k1+k2),allocatable :: i(:)
   end type
end module

program d353689

  use m
  implicit none

  type(base), parameter :: tc = base(null())
  type(base) ::t
  allocate(integer(tc%k1+tc%k2) :: t%i(2))
  print *,t%k1,t%k2,t%i%kind,kind(t%i)

end

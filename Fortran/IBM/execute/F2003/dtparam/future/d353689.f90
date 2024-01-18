! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/20/2009
!*
!*  DESCRIPTION                : test case to track defect 353689
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

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

  type(base) ::t
  allocate(integer(t%k1+t%k2) :: t%i(2))
  if ((t%k1 /= 2) .or. (t%k2 /= 2)) stop 1

  if ((t%i%kind /= 4) .or. (kind(t%i) /= 4)) stop 2

end

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353692.f
!*
!*  DATE                       : July 14 2008 (edited on August 20, 2009)
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  track defect 353821
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353692 (dup of 353821)
!*
!*  CHANGES:
!*  The case tests the KIND of both type parameters and expressions involving them.
!*  As declared below, each kind parameter can have a kind different from the integer
!*  default.  "i" is declared as:
!*    integer(kind(k1+k2)),allocatable :: i(:)
!*  which means the KIND of the sum of k1 and k2 is important, which depends on the
!*  KIND of k1 and the KIND of k2.  Essentially, "kind(k1+k2)" is the maximum of the
!*  KIND of k1 (1) and the KIND of k2 (2 in the example).  In the example, kind(k1+k2) == 2.
!*
!*  It seems that IF statements may handle the KIND information differently than
!*  print statements, so we augment the print statements with IF statements.

!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,k2)
      integer(1),kind :: k1
      integer(k1),kind :: k2
      integer(kind(k1+k2)),allocatable :: i(:)
   end type
end module

program d353692

  use m
  implicit none

  type(base(2,2)) ::t
  print *,t%k1,t%k2
  print *,t%k1%kind,kind(t%k1)
  print *,t%k2%kind,kind(t%k2)
  print *,kind(t%k1+t%k2),t%i%kind,kind(t%i)
  allocate(integer(kind(t%k1+t%k2)) :: t%i(2))

  if ( t%k1 /= 2 .or. t%k2 /= 2 ) stop 2
  if ( t%k1%kind /= 1 .or. kind(t%k1) /= 1 ) stop 3
  if ( t%k2%kind /= 2 .or. kind(t%k2) /= 2 ) stop 4
  if ( kind(t%k1+t%k2) /= 2 .or. t%i%kind /= 2 .or. kind(t%i) /= 2 ) stop 5

end

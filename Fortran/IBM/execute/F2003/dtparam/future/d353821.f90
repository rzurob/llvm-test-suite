!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 16 2008 (edited on August 20, 2009)
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
!* 2. DEFECT 353821
!*
!*  CHANGES:
!*  The case tests the KIND of both type parameters and expressions involving them.
!*  As declared below, each kind and len parameter can have a kind different from
!*  the integer default.  "c" was is declared as:
!*    character(kind(k1+k2)) :: c(kind(k1+k2))
!*  which means the KIND of the sum of k1 and k2 is important, which depends on the
!*  KIND of k1 and the KIND of k2.  Essentially, "kind(k1+k2)" is the maximum of the
!*  KIND of k1 (1) and the KIND of k2 (2 in the example).  In the example, kind(k1+k2) == 2.
!*
!*  It seems that IF statements may handle the KIND information differently than
!*  print statements, so we augment the print statements with IF statements.

!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
      integer(1),kind :: k1=2
      integer(k1),kind :: k2=2

      integer(k1),len :: l1=k1
      integer(k2),len :: l2=k2

      character(kind(k1+k2)) :: c(kind(k1+k2))
    end type
end module

  program d353821
  use m
  implicit none

  type(base(2)) ::t

  print *,t%c%len,len(t%c)
  print *,ubound(t%c,1),kind(t%k1+t%k2)

  if (t%c%len /= 2) stop 2
  if (len(t%c) /= 2) stop 3
  if (ubound(t%c,1) /= 2) stop 4
  if (kind(t%k1+t%k2) /= 2) stop 5

  end

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353423.f
!*
!*  DATE                       : July 7 2008
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
!* 2. DEFECT 353423
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,k3,l1,l2)
      integer(1),kind  :: k1
      integer(k1),kind  :: k2
      integer(k1+k2),kind :: k3
      integer(k3%kind),len :: l1
      integer(kind(k3)),len :: l2
      integer(k2%kind),allocatable :: i1(:)

   end type
end module

  program d353423
  use m
  implicit none
  type(base(2,2,1,1,1)) ::t
  end


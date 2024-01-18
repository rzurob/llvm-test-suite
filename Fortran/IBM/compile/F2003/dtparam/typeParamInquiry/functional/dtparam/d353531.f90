!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353531.f
!*
!*  DATE                       : July 9 2008
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
!* 2. DEFECT 353531
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base1(k1,k2)
        integer,kind          :: k1
        integer(k1),kind      :: k2
        logical(k2%kind)      :: l
   end type

   type base2(k1,k2)
        integer(1),kind       :: k1
        integer(k1),kind      :: k2
        logical(k2%kind)      :: l
   end type
end module

  program d353531
  use m
  implicit none

  type(base1(2,2)) :: b1
  type(base2(2,2)) :: b2

  end

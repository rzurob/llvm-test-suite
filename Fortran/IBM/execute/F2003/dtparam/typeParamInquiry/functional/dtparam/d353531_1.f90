!*********************************************************************
!*  ===================================================================
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
!* 2. DEFECT d353531
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base1(k1,k2)
        integer(1),kind       :: k1
        integer(k1),kind      :: k2
        logical(k2%kind)      :: l
   end type
   type base2(k1,k2)
        integer,kind          :: k1
        integer(k1),kind      :: k2
        logical(k2%kind)      :: l

   end type

end module

program d353531_1
  use m
  implicit none

   type(base1(2,2))  :: t1
   type(base2(2,2))  :: t2
   print *,t1%k2%kind,kind(t1%k2)
   print *,t1%k2
   print *,t1%l%kind,kind(t1%l)

   print *,t2%k2%kind,kind(t2%k2)
   print *,t2%k2
   print *,t2%l%kind,kind(t2%l)

end


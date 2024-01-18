!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 2 2008
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
!* 2. DEFECT 353268
!*
!234567890123456789012345678901234567890123456789012345678901234567890
   module m
     type :: t(k1,l1,l2,l3)
          integer,kind :: k1
          integer(2*k1),len :: l1
          integer(k1+k1),len :: l2
          integer(kind(k1)),len :: l3
     end type
   end module
    program d353268
    use m
    implicit none

    type(t(1,1,1,1)) :: t
    end


!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : September 08 2008
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
!* 2. DEFECT d353994
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(8),len :: l
   end type

   contains
      subroutine check1(b)
        type(base(*)) :: b
        print *,"2--l=",b%l
      end subroutine

end module

program d353994_2
  use m
  implicit none

  type(base(2))  :: b1
  print *,"1--l=",b1%l
  call check1(b1)
  print *,"3--l=",b1%l

end


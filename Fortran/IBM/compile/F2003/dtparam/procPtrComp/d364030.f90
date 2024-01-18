!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : d364030
!*
!*  DATE                       : 2009-04-07
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : defect uncovered in PPC work: parenthesized reference to character DTP component in allocate
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpPPCPassKindLenFunDTPMixedArgsContained
!*
!*  DESCRIPTION
!*
!*  Try to allocate with parenthesized expression as source.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module parenthesizedSource

  implicit none

  type dt (l)
     integer, len  :: l
     character(l)  :: chval2
  end type dt

contains

  subroutine catVal2(this)
    class(dt(*)), intent(in) :: this
    character(:), pointer :: catVal2a
    allocate(catVal2a, source= (this%chval2))
  end subroutine

end module parenthesizedSource

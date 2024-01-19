!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-04-16
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure Pointers as Components
!*
!*  SECONDARY FUNCTIONS TESTED : verify that the rules in 12.3.1.1 requiring an explicit interface are obeyed
!*
!*  REFERENCE                  : Feature Number 363426
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  12.3.1.1 requires an explicit interface for a procedure pointer if
!*  "(3) The procedure has a result that
!*       (a) is an array,
!*       (b) is a pointer or is allocatable, or
!*       (c) has a nonassumed type parameter value that is not an initialization expression, ..."
!*  We set up a procedure component with a bad implicit interface which the compiler
!*  should detect.  Only the last rule, (c), is applicable to DTP, so we only test it.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPPCExplicitInterfaceRequiredmod

  implicit none

  type dt (k)
     integer, kind :: k
     procedure (integer(k)), pointer, nopass   :: pOk1 => null() ! okay
     procedure (real(k)), pointer, nopass      :: pOk2 => null() ! also okay
     procedure (character(4)), pointer, nopass :: pOk3 => null() ! also okay
     procedure (character(k)), pointer, nopass :: pOk4 => null() ! weird, but also okay
  end type dt

  type dl (l)
     integer, len  :: l
     procedure (character(l)), pointer, nopass :: pNo1 => null() ! bad - L must be initialization expression
  end type dl

end module dtpPPCExplicitInterfaceRequiredmod

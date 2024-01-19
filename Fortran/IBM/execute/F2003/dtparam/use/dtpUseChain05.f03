!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : using used modules
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A much simpler version of the "chain" series:
!*  A single module defines two parameterised derived types which are USEd by
!*  the main program, creating two names for each and attempting to use them
!*  interchangeably.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module base

  type string(l)
     integer, len :: l
     character(l) :: datum = 'blank'
  end type string

  type number(k)
     integer, kind :: k
     integer(k) :: datum = 9999
  end type number

end module base


program dtpUseChain05

  use base, only: zwirn => string, string
  use base, only: ordinal => number, number

  type(zwirn(4))    :: z4, z4a
  type(string(4))   :: s4, s4a
  type(number(4))   :: n4, n4a
  type(ordinal(4))  :: o4, o4a


  z4  = zwirn(4)('kangaroo')
  s4  = string(4)('tigger')
  z4a = string(4)('kangaroo')
  s4a = zwirn(4)('tigger')

  n4  = number(4)(123)
  o4  = ordinal(4)(234)
  n4a = ordinal(4)(345)
  o4a = number(4)(456)

  print *, z4, z4a, s4, s4a, n4, n4a, o4, o4a

end program dtpUseChain05

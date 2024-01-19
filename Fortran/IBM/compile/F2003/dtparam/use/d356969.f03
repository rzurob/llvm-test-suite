!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-01
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE (actually, internal function prefix)
!*
!*  SECONDARY FUNCTIONS TESTED : misc
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
!*  A declaration type spec in a function prefix cannot depend on the value of
!*  a dummy argument.
!*  (Originally for defect 356969, but now recast as a diagnostic.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


program d356969
  implicit none

  if (len(fun(3)) /= 3) error stop 10
  if (fun(5) /= 'abcde') error stop 11

contains

  character(n) function fun(n)
    integer n
    fun = 'abcdefghijkl'
  end function fun

end program d356969

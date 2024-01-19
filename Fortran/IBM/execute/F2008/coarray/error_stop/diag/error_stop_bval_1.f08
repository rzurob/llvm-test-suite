!*
!*  ===================================================================
!*
!*  TYPE                       : Duagnostic test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Test that the error message is issued when
!*                               the ERROR STOP stop code is not scalar constant
!*                               expression of character or integer type
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_bval

  error stop i
  error stop k8
  error stop 1 2
  error stop 3k
  error stop .
  error stop *
  error stop [i]
  error stop 0xZ
  error stop \t
  error stop j=2
  error stop -
  error stop error stop
end program error_stop_bval

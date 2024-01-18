!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : implicit typing rules (var is "declared" via a spec. statement which is not a type)
!*
!*  DESCRIPTION
!*
!*  Just a simple example of implicit typing in the block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BImplType

  ivar = 999
  rvar = 9.09
  print *, ivar, rvar

  block
    ivar = ivar + 3
    rvar = rvar * 10
    qvar = 13.1
    jvar = 4
    print *, ivar, rvar, jvar, qvar
  end block

  print *, ivar, rvar

end program BImplType

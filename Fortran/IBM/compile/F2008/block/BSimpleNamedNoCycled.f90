!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BSimpleNamedNoCycled
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : cannot cycle a block
!*  ADAPTED FROM               : BCycle
!*
!*  DESCRIPTION
!*
!*  CYCLE statements must be in a DO loop and "jump" to the named DO construct when
!*  a name appears in the statement, and otherwise to the nearest containing DO loop.
!*  Unlike the EXIT statement, we can *only* cycle to a DO construct, and not to a
!*  BLOCK.  This case tests the simple case of an attempt to cycle to a named block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BSimpleNamedNoCycled

  implicit none
  logical :: pleaseStop
  B: block
    print *, 'stop on T'
    read *, pleaseStop
    if (.not. pleaseStop) cycle B
  end block B

end program BSimpleNamedNoCycled

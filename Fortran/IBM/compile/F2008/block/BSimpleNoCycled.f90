!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BSimpleNoCycled
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-14
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
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
!*  BLOCK.  This case tests the simple case of an attempt to cycle to an unnamed block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BSimpleNoCycled

  implicit none
  logical :: pleaseStop
  block
    print *, 'stop on T'
    read *, pleaseStop
    if (.not. pleaseStop) cycle
  end block

end program BSimpleNoCycled

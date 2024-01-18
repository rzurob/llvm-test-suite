!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : BVolatile02
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2010-12-14
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : very long variable name in volatile statement in block
!*
!*  DESCRIPTION
!*
!*  This is a freak discovery while attempting to divine properties of generated
!*  WCODE: If you use a variable name longer than that allowed, the compiler is
!*  supposed to truncate it and continue with the truncated name.  VOLATILE
!*  has to be taught this.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BVolatile02
  integer i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy1i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy2i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy3i_abcdefghi
  block
    volatile i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy1i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy2i_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy_abcdefghijklmnopqrstuvwxy3i_abcdefghi
  end block
end program BVolatile02

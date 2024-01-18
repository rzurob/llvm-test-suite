!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : d357014d
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE (actually, diagnostic)
!*
!*  SECONDARY FUNCTIONS TESTED : procedure components
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Component procedures - should complain about tk not being declared.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

  type :: control
     procedure (type(tk)), pointer, nopass :: ptk
  end type control
end

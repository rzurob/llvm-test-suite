!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : d361857
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-10
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : problems due to defect 361857
!*
!*  REFERENCE                  : Feature Number 360669
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
!*  Verify that the correct value is accessed via a pointer to a derived type
!*  target with a length parameter even if the desired component is preceded by
!*  two or more components which depend on the length parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program d361857
  type b(l)
     integer, len :: l
     character(l) :: c
     integer :: x(l)
     integer :: y(l)
  end type b
  type(b(1)), target :: b1
  type(b(:)), pointer :: b1p
  b1 = b(1)('a',1,2)
  b1p => b1
  print *, b1p % c, b1p % x, b1p % y ! expect c 1 2
  print *, b1 % c, b1 % x, b1 % y    ! expect c 1 2
end program d361857

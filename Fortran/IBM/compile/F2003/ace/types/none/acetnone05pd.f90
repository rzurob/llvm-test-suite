!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone05pd
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-28
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : chars must have same length type parameters if no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : array constructor, character, length type parameter
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  ll. 8-10, p. 68: "If type-spec is omitted, each ac-value expression in the
!*  array constructor shall have the same length type parameters; in this case,
!*  the type and type parameters of the array constructor are those of the
!*  ac-value expressions."
!*  - and -
!*  ll. 15-17, loc. cit.: "The character length of an ac-value in an
!*  ac-implied-do whose iteration count is zero shall not depend on the value
!*  of the ac-do-variable and shall not depend on the value of an expression
!*  that is not an initialization expression." (i.e., don't ignore a variable-
!*  length ac-value just because there are no occurances of it)
!*
!*  Verify that using different lengths in the same AC is not permitted in I/O
!*  statements.
!*  NB: The above are in the text and not in a formal constraint, so the
!*  compiler is not compelled to conform to them.  It does reject simple cases
!*  which are obviously of different length, so we continue tests in the same
!*  vein.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05pd

  implicit none

  integer :: i

  print *, (/ 'a', 'b', 'cc' /)
  print *, (/ 'aa', 'bb', 'c' /)
  print *, (/ 'aa', 'bbb', 'cc' /)
  print *, (/ 'aaa', 'bbb', 'cccc' /)

  print *, (/ 1_'d', 1_'e', 1_'' /)
  print *, (/ 1_'', 1_'e', 1_'' /)
  print *, (/ 1_'e', 1_'', 1_'e' /)

  print *, (/ ('a','bb','ccc',i=1,1) /)
  print *, (/ ('a','bb','ccc',i=1,0) /)
  print *, (/ ('a',i=1,3), ('bb',i=1,0) /)

  print *, (/ repeat('a',1), repeat('b',2) /)
  print *, (/ repeat('a',1), repeat('b',2), repeat('b',2) /)

  write(6,*) (/ 'a', 'b', 'cc' /)
  write(6,*) (/ 'aa', 'bb', 'c' /)
  write(6,*) (/ 'aa', 'bbb', 'cc' /)
  write(6,*) (/ 'aaa', 'bbb', 'cccc' /)

  write(6,*) (/ 1_'d', 1_'e', 1_'' /)
  write(6,*) (/ 1_'', 1_'e', 1_'' /)
  write(6,*) (/ 1_'e', 1_'', 1_'e' /)

  write(6,*) (/ ('a','bb','ccc',i=1,1) /)
  write(6,*) (/ ('a','bb','ccc',i=1,0) /)
  write(6,*) (/ ('a',i=1,3), ('bb',i=1,0) /)

  write(6,*) (/ repeat('a',1), repeat('b',2) /)
  write(6,*) (/ repeat('a',1), repeat('b',2), repeat('b',2) /)

end program acetnone05pd

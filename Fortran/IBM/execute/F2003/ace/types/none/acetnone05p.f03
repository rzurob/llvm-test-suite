!******************************************************************************
!*  ===========================================================================
!*
!*  DATE                       : 2006-07-28
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : chars must have same length type parameters if no type spec
!*
!*  REFERENCE                  : Feature Number 289053
!*
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
!*  Verify that using the same lengths in the AC in I/O is permitted.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05p

  implicit none

  integer :: i, ival

  ! The only way to have an empty AC without a type specifier:
  print *, (/ ('a',i=1,0) /)
  print *, (/ (1_'a',i=1,0) /)

  ! Try various constant lengths (all the same within the AC) - all should be okay
  print *, (/ 'a', 'b', 'c' /)
  print *, (/ '', '', '' /)
  print *, (/ 'ab', 'cd', 'ef' /)
  print *, (/ 'abcd', 'efgh', 'ijkl' /)

  ! Add in kind:
  print *, (/ 1_'', 1_'', 1_'' /)
  print *, (/ 1_'abcd', 1_'efgh', 1_'ijkl' /)

  ! Repeats of constant length - all should be okay
  print *, (/ repeat('a',1), repeat('b',1), repeat('c',1) /)
  print *, (/ repeat('a',0), repeat('b',0), repeat('c',0) /)
  print *, (/ repeat('a',4), repeat('b',4), repeat('c',4) /)
  ival = 4
  print *, (/ repeat('a',ival), repeat('b',ival), repeat('c',ival) /)

  print *, (/ ('a',i=1,3) /)
  print *, (/ ('',i=1,3) /)
  print *, (/ ('abcd',i=1,3) /)
  print *, (/ (1_'abcd',i=1,3) /)

  print *, (/ ('gh',i=1,0) /) // (/ ('ij',i=1,0) /)
  print *, (/ 'gh', 'ij', 'kl' /)
  print *, (/ 'g', 'h', 'i' /) // (/ 'j', 'k', 'l' /)
  print *, (/ 'g', 'h', 'i' /) // (/ 'jk', 'lm', 'no' /) // (/ 'p', 'q', 'r' /)

  print *, (/ (repeat('a',4),i=1,3) /)

end program acetnone05p

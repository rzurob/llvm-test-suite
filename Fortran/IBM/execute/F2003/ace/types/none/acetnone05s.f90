!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone05s
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
!*  Verify that using the same lengths in the AC in an assignment statement is
!*  permitted.  Diagnostics appear in separate test cases.
!*
!*  This replicates test case acetnone05, using square brackets.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05s

  implicit none

  character   :: charrDx3 (3), charrDx0(0)
  character(1):: charr1x3(3), charr1x0(0)
  character(2):: charr2x3(3), charr2x0(0)
  integer, parameter :: iconst = 5
  integer :: i, ival

  ! The only way to have an empty AC without a type specifier:
  charrDx0 = [ ('a',i=1,0) ]
  charr1x0 = [ ('a',i=1,0) ]
  charr2x0 = [ ('aa',i=1,0) ]
  print *, charrDx0, charr1x0, charr2x0
  charrDx0 = [ (1_'a',i=1,0) ]
  charr1x0 = [ (1_'a',i=1,0) ]
  charr2x0 = [ (1_'aa',i=1,0) ]
  print *, charrDx0, charr1x0, charr2x0

  ! Try various constant lengths (all the same within the AC) - all should be okay
  charrDx3 = [ 'a', 'b', 'c' ]
  charr1x3 = [ 'a', 'b', 'c' ]
  charr2x3 = [ 'a', 'b', 'c' ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ '', '', '' ]
  charr1x3 = [ '', '', '' ]
  charr2x3 = [ '', '', '' ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ 'ab', 'cd', 'ef' ]
  charr1x3 = [ 'ab', 'cd', 'ef' ]
  charr2x3 = [ 'ab', 'cd', 'ef' ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ 'abcd', 'efgh', 'ijkl' ]
  charr1x3 = [ 'abcd', 'efgh', 'ijkl' ]
  charr2x3 = [ 'abcd', 'efgh', 'ijkl' ]
  print *, charrDx3, charr1x3, charr2x3

  ! Add in kind:
  charrDx3 = [ 1_'', 1_'', 1_'' ]
  charr1x3 = [ 1_'', 1_'', 1_'' ]
  charr2x3 = [ 1_'', 1_'', 1_'' ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ 1_'abcd', 1_'efgh', 1_'ijkl' ]
  charr1x3 = [ 1_'abcd', 1_'efgh', 1_'ijkl' ]
  charr2x3 = [ 1_'abcd', 1_'efgh', 1_'ijkl' ]
  print *, charrDx3, charr1x3, charr2x3

  ! Repeats of constant length - all should be okay
  charrDx3 = [ repeat('a',1), repeat('b',1), repeat('c',1) ]
  charr1x3 = [ repeat('a',1), repeat('b',1), repeat('c',1) ]
  charr2x3 = [ repeat('a',1), repeat('b',1), repeat('c',1) ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ repeat('a',0), repeat('b',0), repeat('c',0) ]
  charr1x3 = [ repeat('a',0), repeat('b',0), repeat('c',0) ]
  charr2x3 = [ repeat('a',0), repeat('b',0), repeat('c',0) ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ repeat('a',4), repeat('b',4), repeat('c',4) ]
  charr1x3 = [ repeat('a',4), repeat('b',4), repeat('c',4) ]
  charr2x3 = [ repeat('a',4), repeat('b',4), repeat('c',4) ]
  print *, charrDx3, charr1x3, charr2x3
  ival = 4
  charrDx3 = [ repeat('a',ival), repeat('b',ival), repeat('c',ival) ]
  charr1x3 = [ repeat('a',ival), repeat('b',ival), repeat('c',ival) ]
  charr2x3 = [ repeat('a',ival), repeat('b',ival), repeat('c',ival) ]
  print *, charrDx3, charr1x3, charr2x3

  charrDx3 = [ ('a',i=1,3) ]
  charr1x3 = [ ('a',i=1,3) ]
  charr2x3 = [ ('ab',i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ ('',i=1,3) ]
  charr1x3 = [ ('',i=1,3) ]
  charr2x3 = [ ('',i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ ('abcd',i=1,3) ]
  charr1x3 = [ ('abcd',i=1,3) ]
  charr2x3 = [ ('abcd',i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3
  charrDx3 = [ (1_'abcd',i=1,3) ]
  charr1x3 = [ (1_'abcd',i=1,3) ]
  charr2x3 = [ (1_'abcd',i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3

  charr2x0 = [ ('gh',i=1,0) ] // [ ('ij',i=1,0) ]
  print *, charr2x0
  charr2x3 = [ 'gh', 'ij', 'kl' ]
  print *, charr2x3
  charr2x3 = [ 'g', 'h', 'i' ] // [ 'j', 'k', 'l' ]
  print *, charr2x3
  charr2x3 = [ 'g', 'h', 'i' ] // [ 'jk', 'lm', 'no' ] // [ 'p', 'q', 'r' ]
  print *, charr2x3

  charrDx3 = [ (repeat('a',4),i=1,3) ]
  charr1x3 = [ (repeat('a',4),i=1,3) ]
  charr2x3 = [ (repeat('a',4),i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3

  charrDx3 = [ (repeat('a',iconst),i=1,3) ]
  charr1x3 = [ (repeat('a',iconst),i=1,3) ]
  charr2x3 = [ (repeat('a',iconst),i=1,3) ]
  print *, charrDx3, charr1x3, charr2x3

end program acetnone05s

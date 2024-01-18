!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone05d
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
!*  Verify that using different lengths in the same AC is not permitted in
!*  assignment statements.
!*  NB: The above are in the text and not in a formal constraint, so the
!*  compiler is not compelled to conform to them.  It does reject simple cases
!*  which are obviously of different length, so we continue tests in the same
!*  vein.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05d

  implicit none

  character   :: charr0(0)
  character   :: charr (3)
  character(1):: charr1(3)
  character(2):: charr2(3)
  integer :: i, ival

  charr2 = (/ 'a', 'b', 'cc' /)
  charr2 = (/ 'aa', 'bb', 'c' /)
  charr2 = (/ 'aa', 'bbb', 'cc' /)
  charr2 = (/ 'aaa', 'bbb', 'cccc' /)

  charr1 = (/ 1_'d', 1_'e', 1_'' /)
  charr1 = (/ 1_'', 1_'e', 1_'' /)
  charr1 = (/ 1_'e', 1_'', 1_'e' /)

  charr2 = (/ ('a','bb','ccc',i=1,1) /)
  charr0 = (/ ('a','bb','ccc',i=1,0) /)
  charr2 = (/ ('a',i=1,3), ('bb',i=1,0) /)

  charr2 = (/ repeat('a',1), repeat('b',1), repeat('c',2) /)
  charr2 = (/ repeat('a',1), repeat('b',2), repeat('c',2) /)
  charr2 = (/ repeat('a',2), repeat('b',1), repeat('c',2) /)

  !!! By rights, the following ought to be illegal, but the stricture on this is
  !!! only in text, and not within a formal constraint, so we don't test these:
  !
  ! charr2 = (/ (repeat('a',i),i=1,3) /)
  ! charr0 = (/ (repeat('a',i),i=1,0) /)
  ! ival = 4
  ! charr0 = (/ (repeat('a',ival),i=1,0) /)
  !
  !!! Creating and testing a function which returns variable length strings
  !!! (i.e., just like REPEAT, but user-defined) is also verboten:
  !
  ! charr0 = (/ (varchar('a',ival),i=1,0) /)
  ! charr2 = (/ (varchar('a',i),i=1,3) /)
  !
  !!! Things get really complicated if you consider an expression like
  !!!   (/ (repeat('a',i),i=1,j) /)
  !!! because it's only illegal if j is not 1, but we may not know that at
  !!! compile time.
  !!! Another interesting one is this, but it's probably too much trouble
  !!! for the compiler to diagnose:
  !
  ! do ival = 3,4
  !   charr2 = (/ repeat('a',ival+1), repeat('b',ival), repeat('b',ival-1) /)
  ! end do

end program acetnone05d

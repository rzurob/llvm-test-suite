!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone05c
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
!*  Verify that using the same lengths in the AC in a procedure call is
!*  permitted.  Diagnostics appear in separate test cases.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05c

  implicit none

  integer, parameter :: iconst = 5
  integer :: i, ival

  ! The only way to have an empty AC without a type specifier:
  call sub((/ ('a',i=1,0) /))
  call sub((/ ('aa',i=1,0) /))

  call sub((/ (1_'a',i=1,0) /))
  call sub((/ (1_'aa',i=1,0) /))

  ! Try various constant lengths (all the same within the AC) - all should be okay
  call sub((/ 'a', 'b', 'c' /))
  call sub((/ '', '', '' /))
  call sub((/ 'ab', 'cd', 'ef' /))
  call sub((/ 'abcd', 'efgh', 'ijkl' /))

  ! Add in kind:
  call sub((/ 1_'', 1_'', 1_'' /))
  call sub((/ 1_'abcd', 1_'efgh', 1_'ijkl' /))


  ! Repeats of constant length - all should be okay
  call sub((/ repeat('a',1), repeat('b',1) /))
  call sub((/ repeat('a',0), repeat('b',0), repeat('c',0), repeat('d',0) /))
  call sub((/ repeat('a',4), repeat('b',4), repeat('c',4) /))
  do ival = 3,4
     call sub((/ repeat('a',ival), repeat('b',ival) /))
  end do

  call sub((/ ('a',i=1,3) /))
  call sub((/ ('ab',i=1,3) /))
  call sub((/ ('',i=1,3) /))
  call sub((/ ('abcd',i=1,3) /))
  call sub((/ (1_'abcd',i=1,3) /))

  call sub((/ ('gh',i=1,0) /) // (/ ('ij',i=1,0) /))
  call sub((/ 'g', 'h', 'i' /) // (/ 'j', 'k', 'l' /))
  call sub((/ 'g', 'h', 'i' /) // (/ 'jk', 'lm', 'no' /) // (/ 'p', 'q', 'r' /))

  call sub((/ (repeat('a',4),i=1,2) /))
  call sub((/ (repeat('a',iconst),i=1,4) /))
  do ival = 1,3
     call sub((/ (repeat('a',iconst),i=1,ival) /))
  end do

contains

  subroutine sub(arg)
    character(*) :: arg(:)
    print *, size(arg), len(arg), arg
  end subroutine sub

end program acetnone05c

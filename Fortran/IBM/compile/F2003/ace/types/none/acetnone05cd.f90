!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetnone05cd
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
!*  Verify that using different lengths in the AC in a procedure call is not
!*  permitted.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone05cd

  implicit none

  integer :: i, ival

  call sub((/ ('a','aa',i=1,0) /))
  call sub((/ (1_'aa',1_'a',i=1,0) /))

  call sub((/ 'a', 'b', 'cc' /))
  call sub((/ 'a', 'bb', 'c' /))
  call sub((/ 'aa', 'b', 'c' /))
  call sub((/ 'aa', 'bb', 'c' /))
  call sub((/ '', 'a', '' /))
  call sub((/ 'a', '', 'a' /))

  call sub((/ repeat('a',1), repeat('b',2) /))
  call sub((/ repeat('a',1), repeat('b',2), repeat('c',2) /))

  call sub((/ ('a','bb',i=1,3) /))
  call sub((/ ('','aaa',i=1,3) /))
  call sub((/ ('abcd','',i=1,3) /))
  call sub((/ (1_'abcd',1_'',i=1,3) /))

  call sub((/ ('gh','a',i=1,0) /) // (/ ('a','ij',i=1,0) /))
  call sub((/ 'g', 'hh', 'i' /) // (/ 'jj', 'k', 'l' /))
  call sub((/ 'g', 'h', 'i' /) // (/ 'j', 'lm', 'no' /) // (/ 'p', 'q', 'rr' /))

contains

  subroutine sub(arg)
    character(*) :: arg(:)
    print *, size(arg), len(arg), arg
  end subroutine sub

end program acetnone05cd

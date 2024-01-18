!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt17d
!*
!*  DATE                       : 2006-07-05
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Type specifier syntax (derived type)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : syntax, type specifier, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that non-initial type specifiers are flagged as errors, e.g., more
!*  than one in a row, a type specifier following the first data element, etc.
!*  Also verify that an empty constructor lacking a type specifier is flagged
!*  as an error.
!*
!*  A corresponding set of tests which should work are given in acesynt10.f
!*  This set of diagnostics is for a derived type.  Other tests cover the other types.
!*
!*  The test is successful if the errors are flagged correctly.
!*
!*  Change History:
!*  20060922 dforster While correcting the verification file to conform to
!*                    recently dropped code, it became apparent that the last
!*                    line generated a serious error before line end which
!*                    terminated error checks unexpectedly early.  Consequently,
!*                    the last line has been broken up to allow more thorough
!*                    testing for the acceptability of an embedded AC of a
!*                    different type, sometimes compatible, and sometimes not.
!*                    Also added tests of double complex and double precision.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod
  type derived
     character :: c
  end type derived
end module mod

program acesynt17d

  use mod
  implicit none
  type (derived) :: darr (3), dempty(0)

  ! type specifier repeated:
  darr = (/ derived:: derived:: derived('a'), derived('b'), derived('c') /)
  darr = (/ derived:: derived('a'), derived:: derived('b'), derived('c') /)
  darr = (/ derived:: derived('a'), derived('b'), derived:: derived('c') /)
  darr = (/ derived:: derived('a'), derived('b'), derived('c') derived:: /)
  dempty = (/ /)
  dempty = (/ derived:: derived:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  darr = (/ (/derived:: derived('a')/), (/real::/),             (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/integer::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/double complex::/),   (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/double precision::/), (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/complex::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/logical::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ (/derived:: derived('a')/), (/character::/),        (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)

  darr = (/ derived:: (/derived:: derived('a')/), (/real::/),             (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/integer::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/double complex::/),   (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/double precision::/), (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/complex::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/logical::/),          (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)
  darr = (/ derived:: (/derived:: derived('a')/), (/character::/),        (/derived:: derived('b')/), (/(/derived:: derived('c')/)/) /)

end program acesynt17d


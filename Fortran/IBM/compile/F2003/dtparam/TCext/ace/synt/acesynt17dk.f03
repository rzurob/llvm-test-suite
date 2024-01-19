! GM DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/ace/synt/acesynt17d.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-07 (original: 2006-07-05)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
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
!*  This set of diagnostics is for a derived type.  Other tests cover the
!*  other types.
!*
!*  The test is successful if the errors are flagged correctly.
!*
!*  Change History:
!*  20060922 dforster While correcting the verification file to conform to
!*  recently dropped code, it became apparent that the last line generated
!*  a serious error before line end which terminated error checks
!*  unexpectedly early.  Consequently, the last line has been broken
!*  up to allow more thorough testing for the acceptability of an
!*  embedded AC of a different type, sometimes compatible, and sometimes
!*  not.  Also added tests of double complex and double precision.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type derived(k1,l1)    ! (4,1)
     integer, kind :: k1
     integer, len  :: l1
     character(l1) :: c
  end type derived
end module mod

program acesynt17dk

  use mod
  implicit none
  type (derived(4,1)) :: darr (3), dempty(0)

  ! type specifier repeated:
  darr = (/ derived(4,1):: derived(4,1):: derived(4,1)('a'), derived(4,1)('b'), derived(4,1)('c') /)
  darr = (/ derived(4,1):: derived(4,1)('a'), derived(4,1):: derived(4,1)('b'), derived(4,1)('c') /)
  darr = (/ derived(4,1):: derived(4,1)('a'), derived(4,1)('b'), derived(4,1):: derived(4,1)('c') /)
  darr = (/ derived(4,1):: derived(4,1)('a'), derived(4,1)('b'), derived(4,1)('c'), derived(4,1):: /)
  darr = (/ derived(4,1):: derived(4,1)('a'), derived(4,1)('b'), derived(4,1)('c') derived(4,1):: /)
  dempty = (/ /)
  dempty = (/ derived(4,1):: derived(4,1):: /)

  ! Even though the nested constructors are empty, this is an error, since the types need to be compatible.
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/real::/),             (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/integer::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/double complex::/),   (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/double precision::/), (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/complex::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/logical::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ (/derived(4,1):: derived(4,1)('a')/), (/character::/),        (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)

  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/real::/),             (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/integer::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/double complex::/),   (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/double precision::/), (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/complex::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/logical::/),          (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)
  darr = (/ derived(4,1):: (/derived(4,1):: derived(4,1)('a')/), (/character::/),        (/derived(4,1):: derived(4,1)('b')/), (/(/derived(4,1):: derived(4,1)('c')/)/) /)

end program acesynt17dk

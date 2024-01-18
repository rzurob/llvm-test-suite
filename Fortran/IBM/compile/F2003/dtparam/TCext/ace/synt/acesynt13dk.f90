!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-06 (original: 2006-07-05)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement Type
!*                               specifier syntax (real)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
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
!*  This set of diagnostics is for the real type.  Other tests cover the other
!*  types.
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
!*                    Also added tests of double precision, double complex and
!*                    derived type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type derived (kderived_1) ! kderived_1=1
     integer, kind :: kderived_1
     character(kind=kderived_1) :: c
  end type derived
end module mod

program acesynt13dk

  use mod
  implicit none
  real :: rarr (3), rempty(0)

  ! type specifier repeated:
  rarr = (/ real:: real:: 0.0, 1.0, -1.0 /)
  rarr = (/ real:: 0.0, real:: 1.0, -1.0 /)
  rarr = (/ real:: 0.0, 1.0, real:: -1.0 /)
  rarr = (/ real:: 0.0, 1.0, -1.0 real:: /)
  rempty = (/ /)
  rempty = (/ real:: real:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  rarr = (/ (/real:: 0.0/), (/derived(1)::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /) ! tcx: (1)
  rarr = (/ (/real:: 0.0/), (/integer::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ (/real:: 0.0/), (/double complex::/),   (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ (/real:: 0.0/), (/double precision::/), (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ (/real:: 0.0/), (/complex::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ (/real:: 0.0/), (/logical::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ (/real:: 0.0/), (/character::/),        (/real:: 1.0/), (/(/real:: -1.0/)/) /)

  rarr = (/ real:: (/real:: 0.0/), (/derived(1)::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /) ! tcx: (1)
  rarr = (/ real:: (/real:: 0.0/), (/integer::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ real:: (/real:: 0.0/), (/double complex::/),   (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ real:: (/real:: 0.0/), (/double precision::/), (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ real:: (/real:: 0.0/), (/complex::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ real:: (/real:: 0.0/), (/logical::/),          (/real:: 1.0/), (/(/real:: -1.0/)/) /)
  rarr = (/ real:: (/real:: 0.0/), (/character::/),        (/real:: 1.0/), (/(/real:: -1.0/)/) /)

end program acesynt13dk


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1) to invoke with (1)/declare with (1) - 2 changes

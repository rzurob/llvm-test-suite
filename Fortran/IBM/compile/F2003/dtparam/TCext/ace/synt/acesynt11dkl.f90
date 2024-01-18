!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt11dkl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acesynt11d
!*                               by David Forster)
!*  DATE                       : 2007-12-06 (original: 2006-07-05)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement Type
!*                               specifier syntax (double precision)
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!*  This set of diagnostics is for double precision.  Other tests cover the other types.
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
!*                    Also added tests of double complex and derived type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type derived (kderived_1,lderived_1) ! kderived_1,lderived_1=1,1
     integer, kind :: kderived_1
     integer, len :: lderived_1
     character(lderived_1,kderived_1) :: c
  end type derived
end module mod

program acesynt11dkl

  use mod
  implicit none
  double precision :: darr (3), dempty(0)

  ! type specifier repeated:
  darr = (/ double precision:: double precision:: 0.0, 1.0, -1.0 /)
  darr = (/ double precision:: 0.0, double precision:: 1.0, -1.0 /)
  darr = (/ double precision:: 0.0, 1.0, double precision:: -1.0 /)
  darr = (/ double precision:: 0.0, 1.0, -1.0 double precision:: /)
  dempty = (/ /)
  dempty = (/ double precision:: double precision:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  darr = (/ (/double precision:: 0/), (/real::/),           (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/integer::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/double complex::/), (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/complex::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/logical::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/character::/),      (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ (/double precision:: 0/), (/derived(1,1)::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /) ! tcx: (1,1)

  darr = (/ double precision:: (/double precision:: 0/), (/real::/),           (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/integer::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/double complex::/), (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/complex::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/logical::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/character::/),      (/double precision:: 1/), (/(/double precision:: -1/)/) /)
  darr = (/ double precision:: (/double precision:: 0/), (/derived(1,1)::/),        (/double precision:: 1/), (/(/double precision:: -1/)/) /) ! tcx: (1,1)

end program acesynt11dkl


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1,lderived_1) to invoke with (1,1)/declare with (1,1) - 2 changes

! GM DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/ace/synt/acesynt18d.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt18dc
!*
!*                               by David Forster)
!*  DATE                       : 2007-12-07 (original: 2006-09-22)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
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
!*  This set of diagnostics is for double complex.  Other tests cover the
!*  other types.
!*
!*  The test is successful if the errors are flagged correctly.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type derived(k1,l1)    ! (1,1)
     integer, kind             :: k1
     integer, len              :: l1
     character(kind=k1,len=l1) :: c
  end type derived
end module mod

program acesynt18dc

  use mod
  implicit none
  double complex :: darr (3), dempty(0)

  ! type specifier repeated:
  darr = (/ double complex:: double complex:: 0.0, 1.0, -1.0 /)
  darr = (/ double complex:: 0.0, double complex:: 1.0, -1.0 /)
  darr = (/ double complex:: 0.0, 1.0, double complex:: -1.0 /)
  darr = (/ double complex:: 0.0, 1.0, -1.0 double complex:: /)
  dempty = (/ /)
  dempty = (/ double complex:: double complex:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  darr = (/ (/double complex:: 0/), (/real::/),             (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/integer::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/double precision::/), (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/complex::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/logical::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/character::/),        (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ (/double complex:: 0/), (/derived(1,1)::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)

  darr = (/ double complex:: (/double complex:: 0/), (/real::/),             (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/integer::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/double precision::/), (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/complex::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/logical::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/character::/),        (/double complex:: 1/), (/(/double complex:: -1/)/) /)
  darr = (/ double complex:: (/double complex:: 0/), (/derived(1,1)::/),          (/double complex:: 1/), (/(/double complex:: -1/)/) /)

end program acesynt18dc

!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt16d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-07-05
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Type specifier syntax (logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
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
!*  This set of diagnostics is for the logical type.  Other tests cover the other types.
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
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

module mod
  type derived
     character :: c
  end type derived
end module mod

program acesynt16d

  use mod
  implicit none
  logical :: larr (3), lempty(0)

  ! type specifier repeated:
  larr = (/ logical:: logical:: .false., .true., .false. /)
  larr = (/ logical:: .false., logical:: .true., .false. /)
  larr = (/ logical:: .false., .true., logical:: .false. /)
  larr = (/ logical:: .false., .true., .false. logical:: /)
  lempty = (/ /)
  lempty = (/ logical:: logical:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  larr = (/ (/logical:: .false./), (/real::/),             (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/integer::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/double complex::/),   (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/double precision::/), (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/complex::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/derived::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ (/logical:: .false./), (/character::/),        (/logical:: .true./), (/(/logical:: .false./)/) /)

  larr = (/ logical:: (/logical:: .false./), (/real::/),             (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/integer::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/double complex::/),   (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/double precision::/), (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/complex::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/derived::/),          (/logical:: .true./), (/(/logical:: .false./)/) /)
  larr = (/ logical:: (/logical:: .false./), (/character::/),        (/logical:: .true./), (/(/logical:: .false./)/) /)

end program acesynt16d


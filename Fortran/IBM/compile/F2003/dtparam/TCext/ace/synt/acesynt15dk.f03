! GM DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/ace/synt/acesynt15d.f

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
!*  This set of diagnostics is for the character type.  Other tests cover the
!*  other types.
!*
!*  The test is successful if the errors are flagged correctly.
!*
!*  Change History:
!*  20060922 dforster While correcting the verification file to conform to
!*  recently dropped code, it became apparent that the last
!*  line generated a serious error before line end which
!*  terminated error checks unexpectedly early.  Consequently,
!*  the last line has been broken up to allow more thorough
!*  testing for the acceptability of an embedded AC of a
!*  different type, sometimes compatible, and sometimes not.
!*  Also added tests of double precision, double complex and derived type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod
  type derived(k1,l1)    ! (4,1)
     integer, kind :: k1
     integer, len  :: l1
     character(l1) :: c
  end type derived
end module mod

program acesynt15dk

  use mod
  implicit none
  character :: carr (3), cempty(0)

  ! type specifier repeated:
  carr = (/ character:: character:: 'a', 'b', 'c' /)
  carr = (/ character:: 'a', character:: 'b', 'c' /)
  carr = (/ character:: 'a', 'b', character:: 'c' /)
  carr = (/ character:: 'a', 'b', 'c' character:: /)
  cempty = (/ /)
  cempty = (/ character:: character:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  carr = (/ (/character:: 'a'/), (/real::/),             (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/integer::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/double complex::/),   (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/double precision::/), (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/complex::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/logical::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ (/character:: 'a'/), (/derived(4,1)::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)

  carr = (/ character:: (/character:: 'a'/), (/real::/),             (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/integer::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/double complex::/),   (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/double precision::/), (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/complex::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/logical::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)
  carr = (/ character:: (/character:: 'a'/), (/derived(4,1)::/),          (/character:: 'b'/), (/(/character:: 'c'/)/) /)

end program acesynt15dk

! GM DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/ace/synt/acesynt14d.f

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
!*  This set of diagnostics is for the complex type.  Other tests cover the
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
  type derived(k1,l1)    ! (1,1)
     integer, kind             :: k1
     integer, len              :: l1
     character(kind=k1,len=l1) :: c
  end type derived
end module mod

program acesynt14dc

  use mod
  implicit none
  complex :: zarr (3), zempty(0)

  ! type specifier repeated:
  zarr = (/ complex:: complex:: (0.0,0.0), (1.0,1.0), (-1.0,-1.0) /)
  zarr = (/ complex:: (0.0,0.0), complex:: (1.0,1.0), (-1.0,-1.0) /)
  zarr = (/ complex:: (0.0,0.0), (1.0,1.0), complex:: (-1.0,-1.0) /)
  zarr = (/ complex:: (0.0,0.0), (1.0,1.0), (-1.0,-1.0) complex:: /)
  zempty = (/ /)
  zempty = (/ complex:: complex:: /)

  ! Even though the nested constructors are empty, this is an error,
  ! since the types need to be compatible.
  zarr = (/ (/complex:: (0.0,0.0)/), (/real::/),             (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/integer::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/double complex::/),   (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/double precision::/), (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/derived(1,1)::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/logical::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ (/complex:: (0.0,0.0)/), (/character::/),        (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)

  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/real::/),             (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/integer::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/double complex::/),   (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/double precision::/), (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/derived(1,1)::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/logical::/),          (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)
  zarr = (/ complex:: (/complex:: (0.0,0.0)/), (/character::/),        (/complex:: (1.0,1.0)/), (/(/complex:: (-1.0,-1.0)/)/) /)

end program acesynt14dc

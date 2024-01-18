!**********************************************************************
!* ====================================================================
!* XL Fortran Test Case                           IBM INTERNAL USE ONLY
!* ====================================================================
!*
!*  TEST CASE NAME             : d346022
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt476bdl)
!*  DATE                       : 2008-01-18
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DEFECT ABSTRACT            : DTPARAM: ACE: DIAG: ICE: xlfentry in "hashtodr"
!*
!*  DESCRIPTION                :
!*  The Reduced Code (below) is from the extension of a Diagnostic Test
!*  Case for the Array Constructor Enhancement.  When Compiled, this code
!*  ICEs in xlfentry.
!*
!*  NOTE:  In addition to the ICE, the Diagnostic the Compiler emits is
!*  different from the Diagnostic the original Test Case expects:
!*
!*  line 14.16: 1511-142 (S) Identifier derived specified in the array
!*  constructor has not been defined in a derived type definition.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  type, private :: derived(l1)    ! (20)
      integer, len :: l1
  end type derived

  type (derived(20)), public :: dparr0(0)

end module mod

program d346022
  use mod

  dparr0  = (/ derived(20):: /) ! <= Line 14: (probably causes ICE)

end program d346022

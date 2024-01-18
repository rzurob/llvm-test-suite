!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt35bdl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt35bd
!*                               by David Forster)
!*  DATE                       : 2007-11-29 (original: 2006-11-13)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters Array Constructor
!*                               Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement undefined
!*                               variable in derived type AC in print stmt
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : AC, print, undefined variable
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that the compiler correctly handles undefined variables in AC's
!*  in a print statement.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acetdt35bdl

  implicit none
  type ADerived (lADerived_1) ! lADerived_1=7
     integer, len :: lADerived_1
     real(4) :: rpfield
  end type ADerived
  integer :: j
  type (ADerived(7)) :: t(1) ! tcx: (7)

  print *, [(ADerived(7)(rt2), j=1,1)] ! tcx: (7)

end program acetdt35bdl


! Extensions to introduce derived type parameters:
! type: ADerived - added parameters (lADerived_1) to invoke with (7)/declare with (*) - 2 changes

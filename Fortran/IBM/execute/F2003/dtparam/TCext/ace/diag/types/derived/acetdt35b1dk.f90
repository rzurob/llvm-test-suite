!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt35b1dk
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt35b1d
!*                               by David Forster)
!*  DATE                       : 2007-11-29 (original: 2007-09-07
!*                               (from original 2006-11-13))
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
!*  Verify that the compiler correctly handles undefined variables in AC's in
!*  a print statement.  (Identical to acetdt35bd, with the AC-IMP-DO removed.)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acetdt35b1dk

  implicit none
  type ADerived (kADerived_1) ! kADerived_1=4
     integer, kind :: kADerived_1
     real(kADerived_1) :: rpfield
  end type ADerived
  type (ADerived(4)) :: t(1) ! tcx: (4)

  print *, [ADerived(4)(rt2)] ! tcx: (4)

end program acetdt35b1dk


! Extensions to introduce derived type parameters:
! type: ADerived - added parameters (kADerived_1) to invoke with (4)/declare with (4) - 2 changes

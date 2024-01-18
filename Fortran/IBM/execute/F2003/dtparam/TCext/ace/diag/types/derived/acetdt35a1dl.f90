! GM DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/ace/diag/types/derived/acetdt35a1d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt35a1dl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt35a1d
!*                               by David Forster)
!*  DATE                       : 2007-11-20 (original: 2007-09-07
!*                               (from original 2006-11-13))
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : AC, assignment, undefined variable
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the compiler correctly handles undefined variables in AC's in
!*  an assignment statement.  (Identical to acetdt35ad, with the AC-IMP-DO
!*  removed.)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acetdt35a1dl

  implicit none
  type ADerived(l1,k1)    ! (20,4)
     integer, kind :: k1
     integer, len  :: l1
     real(k1)      :: rpfield
  end type ADerived
  type (ADerived(20,4)) :: t(1)

  t = [ADerived(20,4)(rt2)]

end program acetdt35a1dl

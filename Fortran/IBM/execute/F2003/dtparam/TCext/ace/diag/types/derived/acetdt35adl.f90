!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt35adl
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-29 (original: 2006-11-13)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters Array Constructor
!*                               Enhancements
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement undefined
!*                               variable in derived type AC in assignment stmt
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : AC, assignment, undefined variable
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the compiler correctly handles undefined variables in AC's in
!*  an assignment statement.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program acetdt35adl

  implicit none
  type ADerived (lADerived) ! lADerived=3
     integer, len :: lADerived
     real(4) :: rpfield
  end type ADerived
  type (ADerived(3)) :: t(1) ! tcx: (3)
  integer :: j

  t = [(ADerived(3)(rt2), j=1,1)] ! tcx: (3)

end program acetdt35adl


! Extensions to introduce derived type parameters:
! type: ADerived - added parameters (lADerived) to invoke with (3)/declare with (*) - 2 changes

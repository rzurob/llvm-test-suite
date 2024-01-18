!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d346263
!*
!*  DATE                       : 2008-01-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: ACE: AC-IMPDO: ICE: In xlfentry
!*                               (Signal 11)
!*
!*  DESCRIPTION                :
!*  When Compiled, the Reduced Code (below) coredumps in "xlfentry" with
!*  Signal 11 (SIGSEGV).  The coredump appears to be caused by the
!*  ac-implied-do on Line 16.
!*
!*  NOTE:  Confirmed with Chris Tandy prior to opening this Defect; given
!*         that the original Test Case runs successfully -- with the
!*         ac-implied-do.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt55mod

  type parent (kparent_1,lparent_1) ! kparent_1,lparent_1=4,1
     integer, kind :: kparent_1
     integer, len :: lparent_1
     integer(4) :: ival
  end type parent

end module acetdt55mod

program d346263
  use acetdt55mod

  class (parent(4,1)), allocatable :: parr(:) ! tcx: (4,1)

  allocate(parr(1), source=[parent(4,1):: (parent(4,1)(i), i=1,1)]) !<= Line 16

end program d346263

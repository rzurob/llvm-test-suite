!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d346718
!*
!*  DATE                       : 2008-02-04
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : ASSOCIATE:ACE:AC-IMPDO:DIAG:Missing Diagnostic
!*                               with Undeclared ac-do-variable
!*
!*  DESCRIPTION                :
!*  NOTE:  Found this while implementing Test Case changes related to
!*         Defect 344278.  Although this is not a Diagnostic Test Case,
!*         a new (Diagnostic) Test Case will be added to tracking this
!*         failure.  I'll append a note to this Defect with the new Test
!*         Case path.
!*
!*  The Reduced Code (below) specifies "IMPLICIT NONE", then PRINTs a
!*  series of single element Arrays constructed using an ac-implied-do
!*  within an ASSOCIATE Block.  Only the ac-do-variable for the first
!*  Array Constructor has previously been declared.
!*
!*  When this code is compiled, the Compiler should emit a Diagnostic
!*  for both Lines 9 and 10 similar to the following:
!*
!*  line 9.18: 1516-036 (S) Entity j has undefined type.
!*
!*  These Diagnostics are correctly emitted when the ASSOCIATE Block
!*  is commented out.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program d346718
  implicit none

  integer :: i

  associate(foo => .true.)

     print *, [ (i, i=1,1) ]
     print *, [ (j, j=1,1) ]        ! <= Line 9  Entity j has undefined type.
     print *, [ (int, int=1,1) ]    ! <= Line 10 Entity int has undefined type.

  end associate

end program d346718

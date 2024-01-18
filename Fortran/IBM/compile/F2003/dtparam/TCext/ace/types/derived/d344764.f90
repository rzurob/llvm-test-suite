!**********************************************************************
!* ====================================================================
!* XL Fortran Test Case                           IBM INTERNAL USE ONLY
!* ====================================================================
!*
!*  TEST CASE NAME             : d344764
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt01bkkl)
!*
!*  DATE                       : 2007-12-11
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DEFECT ABSTRACT            : DTPARAM: ICE: 1517-005 (U) Error in
!*                               GEN_AST_UNKNOWN.
!*
!*  DESCRIPTION                :
!*  When Compiled the Reduced Code (below) emits the following Diagnostic:
!*
!*  line 12.0: 1517-005 (U) Error in GEN_AST_UNKNOWN.  Call service representative.
!*
!*  This message could be a little more descriptive as to what the problem
!*  really is (a Defered Length Type Parameter in a Structure Constructor).
!*
!*  NOTE:  The error illustrated by the Reduced Code was corrected in the
!*  Test Case noted above as this was not the intent of the Test Case.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

program d344764

  implicit none

  type pContainer (lpContainer_1) ! kpContainer_1,lpContainer_1=4,13
     integer, len :: lpContainer_1
     integer(4) :: id = 0
  end type pContainer

  type (pContainer(13)) :: pc

  pc   = pContainer(:)(8)  ! <= Line 12 - Defered Length Parameter
                           !              in Structure Constructor

end program d344764

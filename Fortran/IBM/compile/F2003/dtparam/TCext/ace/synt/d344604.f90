!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d344604
!*
!*  DATE                       : 2007-12-07
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: DIAG: Kind Type Parameter --
!*                               Invalid KIND number for CHARACTER
!*
!*  DESCRIPTION                :
!*  The Reduced Code below defines a Derived Type with a KIND Type Parameter
!*  that is used to define the KIND for a CHARACTER Component.  The Compiler
!*  emits the following Diagnostic Message:
!*
!*  1514-214 (E) Invalid KIND number for CHARACTER. Assuming default KIND.  Acceptable value(s): 1
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module mod

  type derived (kderived_1) ! kderived_1=1
     integer, kind :: kderived_1
     character(kind=kderived_1) :: c
  end type derived

end module mod

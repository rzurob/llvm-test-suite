!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d346309
!*
!*  DATE                       : 2008-01-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: INITEXP: ACE: DIAG: 1516-044
!*                               Incorrect Output
!*
!*  DESCRIPTION                :
!*  When the Reduced Code (below) is compiled, the Compiler emits the
!*  following Diagnostic:
!*
!*  line 8.55: 1516-044 (S) A conversion from type contained is not permitted.
!*
!*  This Reduced Code should compile without errors.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d346309mod

  type Contained(k1)    ! (4)
     integer, kind :: k1
  end type Contained

  type DerivedEx
     type (Contained(4)) :: cval(1) = [Contained(4):: Contained(4)()] !<= Line 8
  end type DerivedEx

end module d346309mod

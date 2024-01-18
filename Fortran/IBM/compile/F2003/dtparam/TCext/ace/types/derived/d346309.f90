!**********************************************************************
!* ====================================================================
!* XL Fortran Test Case                           IBM INTERNAL USE ONLY
!* ====================================================================
!*
!*  TEST CASE NAME             : d346309
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt57_dpv)
!*  DATE                       : 2008-01-25
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
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

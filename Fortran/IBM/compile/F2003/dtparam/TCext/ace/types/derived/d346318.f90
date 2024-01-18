!**********************************************************************
!* ====================================================================
!* XL Fortran Test Case                           IBM INTERNAL USE ONLY
!* ====================================================================
!*
!*  TEST CASE NAME             : d346318
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
!*  DEFECT ABSTRACT            : DTPARAM: INITEXP: ACE: DIAG: ICE: 1517-005
!*                               (U) Error in AcMake.
!*
!*  DESCRIPTION                :
!*  When compiled, the Reduced Code (below) causes the Compiler to ICE in
!*  "AcMake":
!*
!*  line 8.39: 1517-005 (U) Error in AcMake.  Please contact your Service
!*  Representative.  For more information visit:
!*  http://www.ibm.com/support/docview.wss?uid=swg21110810
!*
!*  The following Diagnostic is expected for Line 8:
!*
!*  line 8.36: 1516-078 (S) Operands must be conformable.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module d346318mod

  type Contained(k1)    ! (4)
     integer, kind :: k1
  end type Contained

  type DerivedSelf
     type (Contained(4)) :: cval(1) = [Contained(4):: ] ! <= Line 8
  end type DerivedSelf

end module d346318mod

!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July 29, 2008
!*
!*  ABSTRACT                   : DTPARAM: ICE: POLY: In xlfcode with Polymorphic
!*                               POINTER Argument in the ASSOCIATED() Intrinsic
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  ORIGINAL TEST CASE         :
!*  F2003/dtparam/associate/assumedDefered/deferedVariableSelector02.f
!*
!*  DESCRIPTION                :
!*  I found this problem while investigating Defect 354406.  I'll assign
!*  this Defect to Fortran FE since it's related to Derived Type Parameters.
!*
!*  When compiled, the Reduced Code (below) causes an ICE in "xlfcode"
!*  with a TRAP (the ICE appears to occur with the ASSOCIATED() Intrinsic
!*  on Line 28).  This code will compile cleanly if:
!*
!*  o  Line 28 is commented out, or
!*  o  The DTP related aspects are removed from the code.
!*
!*  NOTE:  Line 28 is now Line 69 with the addition of this Header.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod1
    IMPLICIT NONE

    TYPE base(l1)
        INTEGER, LEN :: l1
        INTEGER :: cArray
    END TYPE base

    TYPE, EXTENDS(base) :: extended
    END TYPE extended

END MODULE mod1

PROGRAM d354456
    USE mod1
    IMPLICIT NONE

    CLASS(base(:)), POINTER :: basePoly
    CLASS(extended(:)), POINTER :: extendedPoly

    TYPE(extended(2)), TARGET :: extended_2_4 = extended(2)(4)

    extendedPoly => extended_2_4
    basePoly => extendedPoly%base

    PRINT *, ASSOCIATED( basePoly )
    PRINT *, ASSOCIATED(basePoly, extended_2_4%base)
    PRINT *, ASSOCIATED(basePoly, extendedPoly%base) !<=Line 28: Tobey ICEs Here

END PROGRAM d354456

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : d354468
!*
!*  DATE                       : July 29, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  ABSTRACT                   : DTPARAM: ICE: DIAG: POLY: SELTYPE: Invalid Type
!*                               (with Length Parameter) in Type Guard Statement
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
!*  I found this problem while investigating Defect 354406.
!*
!*  When compiled, the Reduced Code (below) ICEs in "xlfentry" on a Type
!*  Guard Statement that specifies an invalid Type with a Length Parameter.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM d354468
    IMPLICIT NONE

    TYPE base(l1)
        INTEGER, LEN :: l1
        INTEGER :: cArray
    END TYPE base

    TYPE(base(1)), TARGET :: doubleExtendedVar = base(1)(4)
    CLASS(base(:)), POINTER :: doubleExtendedArg

    doubleExtendedArg => doubleExtendedVar
    SELECT TYPE ( doubleExtendedArg )
        CLASS IS (bases(*))
            PRINT *, doubleExtendedArg%cArray
    END SELECT

END PROGRAM d354468

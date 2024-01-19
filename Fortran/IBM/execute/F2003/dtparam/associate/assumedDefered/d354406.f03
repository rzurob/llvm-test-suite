!***********************************************************************
!* =====================================================================
!*
!*                               for Defect d354406 by Glen Mateer)
!*  DATE                       : September  9, 2008
!*
!*  ABSTRACT                   : DTPARAM: DATAPTR: tpt values for pointer not
!*                               set (tgt is componen
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  ORIGINAL TEST CASE         :
!*  F2003/dtparam/associate/assumedDefered/deferedVariableSelector02.scenario
!*
!*  DEFECT DESCRIPTION         :
!*  The Reduced Code (below), makes the Pointer Assignment for a polymorphic
!*  pointer of "base" Type ("basePoly2") to the Base Object of a polymorphic
!*  pointer of "extended" Type ("extendedPoly").
!*
!*  When compiled/linked with FAIL OPTIONS, "basePoly2%cArray" references a
!*  zero size array.  If the Array size in the Base type (on Line 7) is
!*  explicitly set to 1, "basePoly2%cArray" correctly references a non-zero
!*  size array.
!*
!*  ADDITIONAL NOTE            :
!*  <Note by jimxia (Xia, Jim (H.)), 2008/09/09 15:04:30, seq: 3 rel: 0
!*   action: modify>
!*  This is a pointer assignment problem.
!*
!*  The following is a variant that doesn't involve polymorphism at all
!*
!*  NOTE:  The code below is the variant specified in the "Additional Note"
!*         (above).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod1
    TYPE base(l1)
        INTEGER, LEN :: l1

        COMPLEX :: cArray( l1 )
    END TYPE base

    type X (n)
        integer, len :: n

        type(base(n)) :: b1
    end type
END MODULE mod1

    USE mod1
    IMPLICIT NONE

    type(base(:)), POINTER :: basePoly2

    type(X(:)), pointer :: x1

    allocate(X(10) :: x1)
    x1%b1%cArray = (3.0,-3.0)
    basePoly2 => x1%b1

    PRINT *, "basePoly2%cArray = (", basePoly2%cArray, ")"

END PROGRAM

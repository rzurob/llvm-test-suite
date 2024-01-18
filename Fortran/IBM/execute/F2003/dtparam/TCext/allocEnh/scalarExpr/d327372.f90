! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/allocEnh/scalarExpr/d327372.f
! opt variations: -qnol

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d327372 - expr is a Scalar and variable is
!*                               an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOC: Logical Expression Evaluates to .TRUE.
!*                               for Array of Derived Type with an ALLOCATABLE
!*                               Component
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Reduced Code below defines a single Element Array of a Derived Type
!*  (with an ALLOCATABLE Component), and initializes that Element.
!*
!*  Test Case FAILs in the evaluation of the Logical Expression of the
!*  ELSE IF Statement of an IF/ELSE IF Construct (the Logical Expression
!*  evaluates to .TRUE. when it should evaluate to .FALSE.).
!*
!*  A similar failure may also be found in PTF2 for v10.1 (060816).
!*
!*  NOTE:  This Defect may be a Duplicate of Defect 327313.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocArrVarScalarExpr03

    TYPE :: tType2(N1,K1)    ! (20,4)
        INTEGER, KIND            :: K1
        INTEGER, LEN             :: N1
        INTEGER(K1), ALLOCATABLE :: t1
    END TYPE tType2

    INTEGER :: i = 1
    TYPE(tType2(20,4)) :: t2( 1 )

!    ALLOCATE( t2( 1 )%t1 )
    t2( 1 )%t1 = 2

    IF ( .FALSE. ) THEN
        STOP 31
    ELSE IF (t2( i )%t1 /= 2) THEN
        PRINT *, i, t2( i )%t1
        STOP 41
    END IF

END PROGRAM allocArrVarScalarExpr03

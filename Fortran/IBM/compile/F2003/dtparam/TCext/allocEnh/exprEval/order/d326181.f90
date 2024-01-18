! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/exprEval/order/d326181.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d326181 - Order of Expression Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ALLOCENH: Error in WCODE with Derived
!*                               Type Array that Contains an ALLOCATABLE
!*                               Component
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Array Intrinsic Assignment, Vector Subscript,
!*                               Derived Type, ALLOCATABLE Component
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below defines an ALLOCATABLE Variable of Derived Type
!*  (with an ALLOCATABLE Component).  When the ALLOCATABLE Component is
!*  used as a Vector Subscript within the expr of an Intrinsic Assignment,
!*  XLFENTRY emits the following Diagnostic Message:
!*
!*  1517-005 (U) Error in WCODE_DATA_TYPE for SYM: * with type 1.  Call
!*  service representative.
!*
!*  The Reduced Code will compile if the Component is non-ALLOCATABLE.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex08

    TYPE tType(N1,K1)    ! (20,4)
        INTEGER, KIND            :: K1
        INTEGER, LEN             :: N1
        INTEGER(K1), ALLOCATABLE :: idx( : )
    END TYPE tType

    TYPE(tType(:,4)), ALLOCATABLE :: typeArrAlloc( : )

    ALLOCATE( tType(20,4) :: typeArrAlloc( 1 ) )
    ALLOCATE(typeArrAlloc( 1 )%idx( 1 ), SOURCE=(/ 1 /))

    typeArrAlloc = typeArrAlloc( typeArrAlloc( 1 )%idx )

END PROGRAM arrayExprVectorIndex08

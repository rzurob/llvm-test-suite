!*  ===================================================================
!*
!*  DATE                       : October  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ALLOCENH: Error in WCODE with Derived
!*                               Type Array that Contains an ALLOCATABLE
!*                               Component
!*  SECONDARY FUNCTIONS TESTED :
!*
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

    TYPE tType
        INTEGER, ALLOCATABLE :: idx( : )
    END TYPE tType

    TYPE(tType), ALLOCATABLE :: typeArrAlloc( : )

    ALLOCATE( typeArrAlloc( 1 ) )
    ALLOCATE(typeArrAlloc( 1 )%idx( 1 ), SOURCE=(/ 1 /))

    typeArrAlloc = typeArrAlloc( typeArrAlloc( 1 )%idx )

END PROGRAM arrayExprVectorIndex08
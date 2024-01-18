!*  ===================================================================
!*
!*                               an Array
!*
!*  DATE                       : October 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: ALLOC: Incorrect Length Type Parameter
!*                               Result for a Scalar Assigned to a Deferred
!*                               Length CHARACTER Array
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below assigns a Scalar CHARACTER Literal to an Allocated
!*  ALLOCATABLE Array of CHARACTER (of Deferred Length).  The Length Type
!*  Parameter for the CHARACTER Array is incorrectly set.
!*
!*  When the CHARACTER Literal is placed inside an Array Constructor (either
!*  Line 44 or 45), the Length Type Parameter is correctly set.
!*
!*  NOTE:  The Length comparison performed below is affected by Interp
!*  Defect d320792.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocArrVarScalarExpr02

    CHARACTER(:), ALLOCATABLE :: defCharArrAlloc( : )

    defCharArrAlloc = [ 'abcde' ]
!    defCharArrAlloc = (/ 'abcde' /)
    PRINT *, LEN( defCharArrAlloc ), "(", defCharArrAlloc, ")"

    defCharArrAlloc = 'ABC'
!    defCharArrAlloc = [ 'ABC' ]
!    defCharArrAlloc = (/ 'ABC' /)
    PRINT *, LEN( defCharArrAlloc ), "(", defCharArrAlloc, ")"

!    IF (LEN( defCharArrAlloc ) /= 3)  CALL zzrc( 132_4 ) ! d320792

END PROGRAM allocArrVarScalarExpr02

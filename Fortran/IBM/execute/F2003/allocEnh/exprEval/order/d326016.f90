!*  ===================================================================
!*
!*  DATE                       : September 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Intrinsic Assignment:  variable with
!*                               Vector Subscript doesn't Conform with expr
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : Array Intrinsic Assignment, Vector Subscript
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Reduced Code below performs an Array Intrinsic Assignment (on Line 14)
!*  where variable uses a Vector Subscript to index elements of the Array.
!*  Given that the Shape of variable is (/ 2 /), and the Shape of expr is
!*  (/ 1 /), a Diagnostic message should probably be emitted.
!*
!*  NOTE:  This failure exists as far back as v8.11 GOLD (030613).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex05

    CHARACTER(1) :: charArrAlloc( 3 )


    charArrAlloc = (/ 'a', 'b', 'c' /)


    PRINT *, charArrAlloc
    PRINT *, charArrAlloc( (/ 1,3 /) )


    !charArrAlloc( 1:2 ) = charArrAlloc( 1:1 )
    charArrAlloc( (/ 1,3 /) ) = charArrAlloc( 1:2 )


    PRINT *
    PRINT *, charArrAlloc( (/ 1,3 /) )
    PRINT *, charArrAlloc

END PROGRAM arrayExprVectorIndex05

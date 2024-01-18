!*  ===================================================================
!*
!*                               Type
!*
!*  DATE                       : September 15, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor using Implied DO with a
!*                               CHARACTER Substring as the ac-value
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  On Line 7 of the Reduced Code below, the Implied DO in Array Constructor
!*  (using CHARACTER Substring as the ac-value) returns Array Elements with
!*  Null values.
!*
!*  NOTE:  This failure also exists in v10.1 GOLD (050928).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocatedArrayCharExpr01

    CHARACTER(4) :: char4Var2 = 'VUTS'

    PRINT *, "'", char4Var2, "'"
    PRINT *, "'", char4Var2( 3: ), "'"
    PRINT *, "'", (/ (char4Var2( 3: ), i = 1, 1) /), "'"
    PRINT *, "'", (/ CHARACTER(2) :: (char4Var2( 3: ), i = 1, 1) /), "'"
    PRINT *, "'", (/ (char4Var2( 3:4 ), i = 1, 1) /), "'"
    PRINT *, "'", (/ char4Var2( 3: ) /), "'"

END PROGRAM allocatedArrayCharExpr01

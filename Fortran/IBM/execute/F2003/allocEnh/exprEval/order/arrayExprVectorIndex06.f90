!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : September 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               Intrinsic Type (Indexed using a Vector
!*                               Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable (Indexed using
!*                               a Vector Subscript), and will have a
!*                               different Shape Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex06

    INTEGER :: idx1( 3 ) = (/ 2, 3, 4 /)
    INTEGER :: idx2( 3 ) = (/ 5, 6, 7 /)

    CHARACTER(2) :: char2Arr( 11 )
    CHARACTER(2), ALLOCATABLE :: chrArr( : )


    DO i = -5, 5
        WRITE(char2Arr( (i + 6) ), '(I2)') i
    END DO


    ALLOCATE(chrArr( -5:5 ), SOURCE=(/ (char2Arr( i ), i = 1, 11) /))


    PRINT 10, chrArr
10  FORMAT(10("'",A2,"', "),"'",A2,"'")


    chrArr(&
        (/ Indx( chrArr(  2 ) ), Indx( chrArr(  1 ) ),&
           Indx( chrArr(  0 ) ), Indx( chrArr( -1 ) ),&
           Indx( chrArr( -2 ) ) /) ) =&
                chrArr( (/ Indx( chrArr( -2 ) ), Indx( chrArr( -1 ) ),&
                           Indx( chrArr(  0 ) ), Indx( chrArr(  1 ) ),&
                                                 Indx( chrArr(  2 ) ) /) )

    PRINT 10, chrArr


    CONTAINS

        INTEGER FUNCTION Indx( chr )
            CHARACTER(2) :: chr

            READ(chr, '(I2)') Indx

        END FUNCTION Indx

END PROGRAM arrayExprVectorIndex06

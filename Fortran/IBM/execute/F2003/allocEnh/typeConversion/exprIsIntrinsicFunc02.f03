!*  ===================================================================
!*
!*                               Conversion
!*
!*  DATE                       : November  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Unallocated ALLOCATABLE Array,
!*  SECONDARY FUNCTIONS TESTED : expr is the Intrinsic Function RESHAPE(),
!*                               and the Array to be Reshaped is an Assumed
!*                               Shape Array
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
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
!*  For a numeric intrinsic assignment statement, variable and expr may have
!*  different numeric types or different kind type parameters, in which case
!*  the value of expr is converted to the type and kind type parameter of
!*  variable according to the rules of Table 7.9.
!*
!*  Table 7.9: Numeric conversion and the assignment statement
!*  Type of variable Value Assigned
!*  integer     INT (expr, KIND = KIND (variable))
!*  real        REAL (expr, KIND = KIND (variable))
!*  complex     CMPLX (expr, KIND = KIND (variable))
!*  Note: The functions INT, REAL, CMPLX, and KIND are the generic functions
!*  defined in 13.7.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsIntrinsicFunc02

    INTEGER(4) :: numCalls = 0

    INTEGER(2), POINTER :: intArrPtr( :,: )
    INTEGER(2), ALLOCATABLE, TARGET :: intArr( :,: )


    intArr =&
        Convert(&
            RESHAPE(REAL(&
                Convert( RESHAPE([ (REAL(i, 8), i = 125, 1, -1) ],&
                                        [ 5,5,5 ]) ), 16), [ 5,5,5 ]) )

    PRINT *

    intArrPtr => intArr
    CALL Check( )


    CONTAINS


        SUBROUTINE Check( )

            k = 126
            DO i = 1, 25
                PRINT '(5I4)', intArrPtr( :,i )

                DO j = 1, 5
                    k = k - 1
                    IF (intArrPtr( j,i ) /= k) CALL zzrc( (100_4 + INT(k, 4)) )
                END DO
            END DO

        END SUBROUTINE Check


        FUNCTION Convert( arr )
            CLASS(*) :: arr( :,:,: )

            INTEGER(2) :: Convert( 5,25 )

            INTEGER(2), TARGET, ALLOCATABLE :: intArrAlloc( :,: )


            numCalls = numCalls + 1


            PRINT *
            PRINT *, numCalls


            SELECT TYPE ( arr )
                TYPE IS ( REAL(8) )
                    intArrAlloc = RESHAPE(arr, [ 5,25 ])

                TYPE IS ( REAL(16) )
                    intArrAlloc = RESHAPE(arr, [ 5,25 ])

                CLASS DEFAULT
                    CALL zzrc( (numCalls * 10_4) )
            END SELECT

            IF (.NOT. ALLOCATED( intArrAlloc ))&
                    CALL zzrc( ((numCalls * 10_4) + 1_4) )


            PRINT *, SIZE(intArrAlloc, 1), SIZE(intArrAlloc, 2)

            IF (SIZE(intArrAlloc, 1) /= 5)&
                    CALL zzrc( ((numCalls * 10_4) + 2_4) )
            IF (SIZE(intArrAlloc, 2) /= 25)&
                    CALL zzrc( ((numCalls * 10_4) + 3_4) )

            intArrPtr => intArrAlloc
            CALL Check( )

            Convert = intArrAlloc

        END FUNCTION Convert

END PROGRAM exprIsIntrinsicFunc02

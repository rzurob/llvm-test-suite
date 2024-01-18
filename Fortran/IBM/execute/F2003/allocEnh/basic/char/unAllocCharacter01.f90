!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : June 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar/Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar/Array of Type CHARACTER
!*                               with the same Shape and Length Type Parameter
!*                               Values
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unAllocCharacter01

    INTEGER(KIND = 4) :: i
    INTEGER, DIMENSION( 1 ) :: arrayShape

    CHARACTER( : ), ALLOCATABLE :: allocScalarChar
    CHARACTER( : ), DIMENSION( : ), ALLOCATABLE :: allocArrayChar


    IF ( ALLOCATED( allocScalarChar ) ) THEN
        PRINT *, "ALLOCATED( allocScalarChar ) != .FALSE."
        CALL zzrc( 10_4 )

    ELSE IF ( ALLOCATED( allocArrayChar ) ) THEN
        PRINT *, "ALLOCATED( allocArrayChar ) != .FALSE."
        CALL zzrc( 20_4 )
    END IF


    allocScalarChar = 'Test Allocation of Scalar CHARACTER'

    IF (.NOT. ALLOCATED( allocScalarChar ) ) THEN
        PRINT *, "ALLOCATED( allocScalarChar ) != .TRUE."
        CALL zzrc( 30_4 )

    ELSE IF (allocScalarChar%len /=&
                LEN( 'Test Allocation of Scalar CHARACTER' )) THEN
        PRINT *, "allocScalarChar = '", allocScalarChar, "'"
        PRINT *, "allocScalarChar%len =", allocScalarChar%len,&
                 "-- should be", LEN( 'Test Allocation of Scalar CHARACTER' )

        CALL zzrc( 40_4 )

    ELSE IF (allocScalarChar /= 'Test Allocation of Scalar CHARACTER') THEN
        PRINT *, "allocScalarChar = '", allocScalarChar, "'"
        PRINT *, "(should be:  'Test Allocation of Scalar CHARACTER')"

        CALL zzrc( 50_4 )
    END IF


    allocArrayChar = (/&
        ((allocScalarChar( :19 ) // "Array" // allocScalarChar( 26: )),&
                        i = 1, 10) /)

    IF (.NOT. ALLOCATED( allocArrayChar ) ) THEN
        PRINT *, "ALLOCATED( allocArrayChar ) != .TRUE."
        CALL zzrc( 60_4 )

    ELSE
        arrayShape = SHAPE( allocArrayChar )
        IF (arrayShape( 1 ) /= 10) THEN
            PRINT *, "SHAPE( allocArrayChar ) = '",&
                    SHAPE( allocArrayChar ), "' (should be 10)"
            CALL zzrc( 70_4 )

        ELSE
            DO i = 1, 10
                IF (allocArrayChar( i )%len /=&
                    LEN( 'Test Allocation of Array CHARACTER' )) THEN
                    PRINT *, "allocArrayChar(", i, ") = '",&
                                allocArrayChar( i ), "'"
                    PRINT *, "allocArrayChar%len =",&
                            allocArrayChar( i )%len, "-- should be",&
                            LEN( 'Test Allocation of Array CHARACTER' )

                    CALL zzrc( (80_4 + i) )

                ELSE IF (allocArrayChar( i ) /=&
                         'Test Allocation of Array CHARACTER') THEN
                    PRINT *, "allocArrayChar(", i, ") = '",&
                                allocArrayChar( i ), "'"
                    PRINT *, "(should be 'Test Allocation of Array CHARACTER')"

                    CALL zzrc( (90_4 + i) )
                END IF
            END DO
        END IF
    END IF

END PROGRAM unAllocCharacter01

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedLBound04 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Intrinsic
!*                               (Non-CHARACTER) Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 7
!*
!*  DRIVER STANZA              : xlf2003
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

MODULE module
    REAL(8), ALLOCATABLE :: realArrayAlloc( :,:,:,:,:,:,: )
END MODULE module


PROGRAM unAllocatedLBound04
    USE module

    INTERFACE
        INTEGER(4) FUNCTION intrinsicAssignment( indexList )
            USE module
            INTEGER :: indexList( 14 )
        END FUNCTION intrinsicAssignment
    END INTERFACE


    INTEGER(4) :: i
    INTEGER(4) :: returnCode

    INTEGER :: value = 1
    INTEGER :: indices( 0:13 ) =&
            (/ -9,0, 0,9, 10,19, 20,29, 30,39, 40,49, 50,59 /)


    IF ( ALLOCATED( realArrayAlloc ) ) CALL zzrc( 10_4 )


    returnCode = intrinsicAssignment( indices )
    IF (returnCode /= 0_4) CALL zzrc( returnCode )


    IF (.NOT. ALLOCATED( realArrayAlloc )) THEN
        CALL zzrc( 30_4 )

    ELSE IF (SIZE( realArrayAlloc ) /= 10**7) THEN
        PRINT *, "SIZE( realArrayAlloc ) =", SIZE( realArrayAlloc )
        CALL zzrc( 40_4 )

    ELSE
        DO i = 1, 7
            IF (SIZE(realArrayAlloc, i) /= 10) THEN
                PRINT *, "SIZE(realArrayAlloc,", i, ") =",SIZE(realArrayAlloc)
                CALL zzrc( (50_4 + i) )

            ELSE IF (LBOUND(realArrayAlloc, i) /=&
                        indices( ((i - 1) * 2) )) THEN
                PRINT *,"indices(",((i - 1) * 2),") =",indices( ((i - 1) * 2) )
                PRINT *,"LBOUND(realArrayAlloc,",i,")=",LBOUND(realArrayAlloc,i)

                CALL zzrc( (60_4 + i) )
            END IF
        END DO
    END IF


    DO i = indices( 12 ), indices( 13 )
        DO j = indices( 10 ), indices( 11 )
            DO k = indices( 8 ), indices( 9 )
                DO l = indices( 6 ), indices( 7 )
                    DO m = indices( 4 ), indices( 5 )
                        DO n = indices( 2 ), indices( 3 )
                            DO o = indices( 0 ), indices( 1 )
                                IF (realArrayAlloc( o,n,m,l,k,j,i ) /=&
                                        (REAL((value * 2.9979_4), 4))) THEN
                                    PRINT *, o, n, m, l, k, j, i
                                    PRINT *, realArrayAlloc( o,n,m,l,k,j,i )
                                    PRINT *, REAL((value * 2.9979_4), 4)

                                    CALL zzrc( 70_4 )
                                END IF

                                value = value + 1
                            END DO
                        END DO
                    END DO
                END DO
            END DO
        END DO
    END DO

END PROGRAM unAllocatedLBound04


INTEGER(4) FUNCTION intrinsicAssignment( indexList )
    USE module

    INTEGER :: indexList( 14 )

    REAL(4) :: realArray( indexList(  1 ):indexList(  2 ),&
                          indexList(  3 ):indexList(  4 ),&
                          indexList(  5 ):indexList(  6 ),&
                          indexList(  7 ):indexList(  8 ),&
                          indexList(  9 ):indexList( 10 ),&
                          indexList( 11 ):indexList( 12 ),&
                          indexList( 13 ):indexList( 14 )   )

    realArray =&
        RESHAPE((/ (REAL(i * 2.9979_4, 4), i = 1, 10**7) /),&
                                    (/ 10,10,10,10,10,10,10 /))


    realArrayAlloc = realArray

    intrinsicAssignment = 0_4

END FUNCTION intrinsicAssignment

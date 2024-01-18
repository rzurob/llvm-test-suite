!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : diffKindVectorIndex02 - Order of Expression
!*                               Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 11, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type REAL
!*  SECONDARY FUNCTIONS TESTED : expr references elements of variable
!*                               (Indexed using a Vector Subscript), and
!*                               will have a different Shape/Length Type
!*                               Parameter Result
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  diffKindVectorIndex02:   Default
!*  diffKindVectorIndex02a:  -qrealsize=4
!*  diffKindVectorIndex02b:  -qrealsize=8
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

PROGRAM diffKindVectorIndex02

    REAL :: realKind
    INTEGER :: defaultRealKind = KIND( realKind )

    REAL, ALLOCATABLE :: realArrAlloc( :,:,: )

    REAL :: realArr( 5,5,5 ) =&
        RESHAPE((/ (((((REAL( i ) * 10.0) + REAL( j ) + (REAL( k ) / 10.0)),&
            k = 1, 5), j = 1, 5), i = 1, 5) /), (/ 5,5,5 /))


    realArrAlloc = realArr
    IF (KIND( realArrAlloc ) /= defaultRealKind) CALL zzrc( 10_4 )
    CALL Dump( 'Assignment (5x5x5):' )


    realArrAlloc = REAL(realArrAlloc( :,:,(/ 1,1,2,3,4,5,5 /) ), 4)
    IF (KIND( realArrAlloc ) /= defaultRealKind) CALL zzrc( 20_4 )
    CALL Dump( 'REAL(4) (5x5x7):' )


    realArrAlloc = REAL(realArrAlloc( (/ 2,3,4 /),:,: ), 8)
    IF (KIND( realArrAlloc ) /= defaultRealKind) CALL zzrc( 30_4 )
    CALL Dump( 'REAL(8) (3x5x7):' )


    realArrAlloc = REAL(realArrAlloc( :,(/ 2,3,4 /),(/ 3,4,5 /) ), 16)
    IF (KIND( realArrAlloc ) /= defaultRealKind) CALL zzrc( 40_4 )
    CALL Dump( 'REAL(16) (3x3x3):' )


    CONTAINS


        SUBROUTINE Dump( title )
            CHARACTER(*) :: title

            CHARACTER(10) :: fmt


            WRITE(fmt, '("(",I2,"(F6.2))")') SIZE(realArrAlloc, 1)

            PRINT *
            PRINT *, title

            DO i = 1, SIZE(realArrAlloc, 3)
                PRINT *

                DO j = 1, SIZE(realArrAlloc, 2)
                    PRINT fmt, realArrAlloc( :,j,i )
                END DO
            END DO

        END SUBROUTINE Dump

END PROGRAM diffKindVectorIndex02

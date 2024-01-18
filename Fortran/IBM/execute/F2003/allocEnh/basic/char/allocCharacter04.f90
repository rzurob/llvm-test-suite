!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : June 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of Type CHARACTER with
!*                               a different Length Type Parameter Value
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

PROGRAM allocCharacter04

    INTEGER( 4 ) :: i
    INTEGER( 4 ) :: j

    INTEGER, PARAMETER :: M = -5
    INTEGER, PARAMETER :: N =  5
    INTEGER, PARAMETER :: O =  10

    CHARACTER( : ), DIMENSION( :,: ), ALLOCATABLE :: allocCharArray

    CHARACTER( 2 ) :: scalarCharData

    CHARACTER( 256 ) :: errMsg


    scalarCharData = 'zy'

    ALLOCATE(CHARACTER( 4 ) :: allocCharArray( M:N,N:O ),&
                                    STAT=iStat, ERRMSG=errMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *)&
            "ALLOCATE(CHARACTER( 4 ) :: allocCharArray) <", iStat, "> ", errMsg
        CALL zzrc( 10_4 )
    END IF

    DO i = N, O
        DO j = M, N
            WRITE(allocCharArray( j,i ), '(I4)') ((i * 1000) + j)
        END DO
    END DO


    allocCharArray = scalarCharData


    IF (.NOT. ALLOCATED( allocCharArray )) THEN
        WRITE(0, *) "ALLOCATED( allocCharArray ) == .FALSE."
        CALL zzrc( 20_4 )

    ELSE
        DO i = N, O
            PRINT *, "allocCharArray( :,", i, "):"
            PRINT 10, allocCharArray( :,i )

            DO j = M, N
                IF (LEN( allocCharArray( j,i ) ) /= 2) THEN
                    WRITE(0, *) "LEN( allocCharArray(", j, ",",&
                                 i, ") ) =", LEN( allocCharArray( j,i ) )
                    WRITE(0, *) "Should be:  4!"

                    CALL zzrc( (30_4 + ((i - 5) * 10) + (j + 5)) )

                ELSE IF (allocCharArray( j,i ) /= scalarCharData) THEN
                    WRITE(0, *) "allocCharArray(", j, ",",&
                                    i, ") =", allocCharArray( j,i )
                    WRITE(0, *) "(should be '", scalarCharData, "')"

                    CALL zzrc( (40_4 + ((i - 5) * 10) + (j + 5)) )
                END IF
            END DO
        END DO
    END IF

10  FORMAT("(",10("'",A4,"',"),"'",A4,"')")

END PROGRAM allocCharacter04

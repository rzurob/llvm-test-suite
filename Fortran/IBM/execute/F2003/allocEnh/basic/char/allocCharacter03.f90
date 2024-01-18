!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : June 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar/Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar/Array of Type CHARACTER
!*                               with different Length Type Parameter Values
!*                               (but the Shape is the same)
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

PROGRAM allocCharacter03

    INTEGER( 4 ) :: i
    INTEGER( 4 ) :: j

    INTEGER, DIMENSION( 2 ) :: allocArrayShape

    CHARACTER( : ), ALLOCATABLE :: allocScalarChar

    CHARACTER( : ), ALLOCATABLE, DIMENSION( :,: ) :: allocArrayChar

    CHARACTER( 10 ), DIMENSION( 5,5 ) :: dataArray

    CHARACTER(LEN = 256) :: msg


    ALLOCATE(allocScalarChar, SOURCE='12345', STAT=iStat, ERRMSG=msg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "ALLOCATE(allocScalarChar, SOURCE='12345') <",&
                                                        iStat, "> ", msg
        CALL zzrc( 10_4 )
    END IF

    ALLOCATE(CHARACTER( 5 ) :: allocArrayChar( 5,5 ), STAT=iStat, ERRMSG=msg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "ALLOCATE(CHARACTER( 5 ) :: allocArrayChar( 5,5 )) <",&
                                                            iStat, "> ", msg
        CALL zzrc( 20_4 )
    END IF


    DO i = 1, 5
        DO j = 1, 5
            WRITE(allocArrayChar( j,i ), '(I5)') (i * 191) + (j * 1000)
            dataArray( j,i ) = allocArrayChar( j,i ) // allocArrayChar( j,i )
        END DO
    END DO


    allocScalarChar = dataArray( 1,1 )
    IF (.NOT. ALLOCATED( allocScalarChar )) THEN
        WRITE(0, *) "ALLOCATED( allocScalarChar ) == .FALSE."
        CALL zzrc( 30_4 )

    ELSE IF (allocScalarChar /= dataArray( 1,1 )) THEN
        WRITE(0, *) " allocScalarChar = '", allocScalarChar, "'"
        WRITE(0, *) "dataArray( 1,1 ) = '",&
                        dataArray( 1,1 ), "' (should be the same)"
        CALL zzrc( 40_4 )
    END IF


    allocArrayChar = dataArray
    IF (.NOT. ALLOCATED( allocArrayChar )) THEN
        WRITE(0, *) "ALLOCATED( allocArrayChar ) == .FALSE."
        CALL zzrc( 50_4 )
    END IF

    allocArrayShape = SHAPE( allocArrayChar )

    DO i = 1, 2
        IF (allocArrayShape( i ) /= 5) THEN
            WRITE(0, *) "allocArrayShape(", i, ") =", allocArrayShape( i )
            WRITE(0, *) "(should be 5))"

            CALL zzrc( 60_4 )
        END IF
    END DO

    DO i = 1, 5
        DO j = 1, 5
            IF (allocArrayChar( j,i ) /= dataArray( j,i )) THEN
                WRITE(0, *) " allocArrayChar( j,i ) = '",&
                                allocArrayChar( j,i ), "'"
                WRITE(0, *) "dataArray( j,i ) = '",&
                             dataArray( j,i ), "' (should be the same)"

                CALL zzrc( (70_4 + ((i - 1_4) * 5_4) + j) )
            END IF
        END DO
    END DO

END PROGRAM allocCharacter03

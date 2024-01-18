!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocCharacter02 - Basic Tests: CHARACTER
!*                               Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : June 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is an Array of Type CHARACTER
!*                               with a different Shape (but the Length Type
!*                               Parameter Values are the same)
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

PROGRAM allocCharacter02

    INTEGER( 4 ) :: i
    INTEGER( 4 ) :: j
    INTEGER( 4 ) :: k

    INTEGER, DIMENSION( 3 ) :: allocArrayShape

    CHARACTER( 4 ), ALLOCATABLE, DIMENSION( :,:,: ) :: allocArray

    CHARACTER( 4 ), DIMENSION( 3,3,3 ) :: dataArray

    CHARACTER( 256 ) :: errMsg


    ALLOCATE(allocArray( 3,1,3 ), STAT=iStat, ERRMSG=errMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "ALLOCATE( allocArray( 3,1,3 ) ) <", iStat, "> ", errMsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 1, 3
        DO j = 1, 3
            DO k = 1, 3
                WRITE(dataArray( k,j,i ), '(I4)')&
                        (((i * 123) + (k * 1000)) + j)
            END DO

            WRITE(allocArray( j,1,i ), '(I4)') ((i * 123) + (j * 1000))
        END DO
    END DO


    allocArray = dataArray


    IF (.NOT. ALLOCATED( allocArray )) THEN
        WRITE(0, *) "ALLOCATED( allocArray ) == .FALSE."
        CALL zzrc( 20_4 )
    END IF


    allocArrayShape = SHAPE( allocArray )

    DO i = 1, 3
        IF (allocArrayShape( i ) /= 3) THEN
            WRITE(0, *) "allocArrayShape(", i, ") =", allocArrayShape( i )
            CALL zzrc( (30_4 + i) )
        END IF
    END DO


    DO i = 1, 3
        DO j = 1, 3
            DO k = 1, 3
                IF (allocArray( k,j,i ) /= dataArray( k,j,i )) THEN
                    WRITE(0, *) "allocArray(", k, ",", j, ",", i, ") =",&
                                allocArray( k,j,i )
                    WRITE(0, *) "dataArray(", k, ",", j, ",", i, ") =",&
                                dataArray( k,j,i )

                    CALL zzrc( (40_4 + (((i * 9_4) + (j * 3_4) + k) ) ) )
                END IF
            END DO
        END DO
    END DO

END PROGRAM allocCharacter02

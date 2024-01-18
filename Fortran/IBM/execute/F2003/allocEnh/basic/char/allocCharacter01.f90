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

PROGRAM allocCharacter01

    INTEGER( 4 ) :: i

    CHARACTER( 10 ), PARAMETER :: charValue = 'abcdefghij'

    CHARACTER( : ), POINTER :: charScalarPtr
    CHARACTER( : ), ALLOCATABLE, TARGET :: allocScalarChar

    CHARACTER( 5 ), DIMENSION( : ), POINTER :: charArrayPtr
    CHARACTER( 5 ), ALLOCATABLE, DIMENSION( : ), TARGET :: allocArrayChar

    CHARACTER( 256 ) :: msg


    ALLOCATE(CHARACTER( 10 ) :: allocScalarChar, STAT=iStat, ERRMSG=msg)
    IF (iStat /= 0) THEN
        PRINT *,&
            "ALLOCATE(CHARACTER( 10 ) :: allocScalarChar) <", iStat, "> ", msg
        CALL zzrc( 10_4 )
    END IF

    charScalarPtr => allocScalarChar
    allocScalarChar = charValue

    ALLOCATE(allocArrayChar( 10 ), STAT=iStat, ERRMSG=msg)
    IF (iStat /= 0) THEN
        PRINT *, "ALLOCATE( allocArrayChar( 10 ) ) <", iStat, "> ", msg
        CALL zzrc( 20_4 )
    END IF

    charArrayPtr => allocArrayChar


    allocArrayChar = allocScalarChar( 3:7 )

    IF (.NOT. ALLOCATED( allocArrayChar )) THEN
        PRINT *, "ALLOCATED( allocArrayChar ) != .TRUE."
        CALL zzrc( 30_4 )

    ELSE IF (.NOT. ASSOCIATED(charArrayPtr, allocArrayChar)) THEN
        PRINT *, "ASSOCIATED(charArrayPtr, allocArrayChar) != .TRUE."
        CALL zzrc( 40_4 )

    ELSE
        DO i = 1, 10
            IF (allocArrayChar( i ) /= allocScalarChar( 3:7 )) THEN
                PRINT *,"allocArrayChar(", i, ") = '", allocArrayChar(i), "'"
                PRINT *,"allocScalarChar( 3:7 ) = '", allocScalarChar(3:7), "'"

                CALL zzrc( (50_4 + i) )
            END IF
        END DO
    END IF


    allocScalarChar = charValue( 6: ) // charValue( :5 )

    IF (.NOT. ALLOCATED( allocScalarChar )) THEN
        PRINT *, "ALLOCATED( allocScalarChar ) != .TRUE."
        CALL zzrc( 60_4 )

    ELSE IF (.NOT. ASSOCIATED(charScalarPtr, allocScalarChar)) THEN
        PRINT *, "ASSOCIATED(charScalarPtr, allocScalarChar) != .TRUE."
        CALL zzrc( 70_4 )

    ELSE
        IF (allocScalarChar /= (charValue( 6: ) // charValue( :5 ))) THEN
            PRINT *,"allocScalarChar = '", allocScalarChar, "'"
            PRINT *,"charValue( 6: ) // charValue( :5 ) = '",&
                        (charValue( 6: ) // charValue( :5 ))

            CALL zzrc( 80_4 )
        END IF
    END IF

END PROGRAM allocCharacter01

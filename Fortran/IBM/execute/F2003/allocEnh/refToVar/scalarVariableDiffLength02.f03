!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of CHARACTER
!*                               with Deferred Length
!*  SECONDARY FUNCTIONS TESTED : expr references variable with a different
!*                               Length Type Parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM scalarVariableDiffLength02

    INTEGER(4) :: i

    CHARACTER(26), PARAMETER :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(64), PARAMETER :: char64 =&
            alphabet // 'abcdefghijklmnopqrstuvwxyz' // '1234567890+='
    CHARACTER(256), PARAMETER :: char256 = char64 // char64 // char64 // char64
    CHARACTER(1024), PARAMETER :: char1Kb =&
            char256 // char256 // char256 // char256

    CHARACTER(:), ALLOCATABLE :: charScalarAlloc


    ALLOCATE(CHARACTER(0) :: charScalarAlloc)

    DO i = 1_4, 26_4
        charScalarAlloc = charScalarAlloc // alphabet( i:i )

        IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( (10_4 + i) )

        PRINT *, i, LEN( charScalarAlloc ), "'", charScalarAlloc, "'"

        IF (LEN( charScalarAlloc ) /= i)        CALL zzrc( (40_4 + i) )
        IF (charScalarAlloc /= alphabet( :i ))  CALL zzrc( (70_4 + i) )
    END DO


    PRINT *


    DEALLOCATE( charScalarAlloc )

    charScalarAlloc = alphabet
    DO i = 26_4, 1_4, -1
        charScalarAlloc = charScalarAlloc( 2:LEN( charScalarAlloc ) )

        IF (.NOT. ALLOCATED( charScalarAlloc ))        CALL zzrc( (110_4 + i) )

        PRINT *, i, LEN( charScalarAlloc ), "'", charScalarAlloc, "'"

        IF (LEN( charScalarAlloc ) /= (i - 1_4))       CALL zzrc( (140_4 + i) )
        IF (charScalarAlloc /= alphabet( (28_4 - i): ))CALL zzrc( (170_4 + i) )
    END DO


    PRINT *


    DEALLOCATE( charScalarAlloc )

    DO i = 1_4, 1024_4
        charScalarAlloc = charScalarAlloc // char1Kb

        IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 200_4 )

        IF (LEN( charScalarAlloc ) /= (i * 1024_4)) THEN
            PRINT *, i, LEN( charScalarAlloc )
            CALL zzrc( 201_4 )
        END IF
    END DO


    DO i = 1024_4, 1_4, -1_4
        charScalarAlloc = charScalarAlloc( :(i * 1024_4) )

        IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 210_4 )

        IF (LEN( charScalarAlloc ) /= (i * 1024_4)) THEN
            PRINT *, i, LEN( charScalarAlloc )
            CALL zzrc( 211_4 )
        END IF
    END DO

END PROGRAM scalarVariableDiffLength02

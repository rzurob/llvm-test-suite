!*  ===================================================================
!*
!*                               CHARACTER Intrinsic Type
!*
!*  DATE                       : September 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               Deferred Length ALLOCATABLE Scalar Dummy
!*                               Argument of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : The corresponding Actual Argument is an
!*                               Allocated/Unallocated ALLOCATABLE
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

PROGRAM deferredLenDummyArg01

    INTERFACE
        SUBROUTINE AssignSub(charScalarArg, failRCBase)
            CHARACTER(:), ALLOCATABLE :: charScalarArg
            INTEGER(4) :: failRCBase
        END SUBROUTINE AssignSub
    END INTERFACE

    CHARACTER(:), ALLOCATABLE :: charScalarAlloc


    ALLOCATE(CHARACTER(31) :: charScalarAlloc)


    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 10_4 )

    CALL AssignSub(charScalarAlloc, 20_4)

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 11_4 )

    PRINT *, 'LEN( charScalarAlloc ) =', LEN( charScalarAlloc )
    PRINT *, "'", charScalarAlloc, "'"

    IF (LEN( charScalarAlloc ) /= 71) CALL zzrc( 12_4 )

    IF (charScalarAlloc /=&
        ('Test with long CHARACTER Var. and ' //&
            '"Test with a long CHARACTER Variable"')) CALL zzrc( 13_4 )


    DEALLOCATE( charScalarAlloc )


    PRINT *

    IF ( ALLOCATED( charScalarAlloc ) ) CALL zzrc( 14_4 )

    CALL AssignSub(charScalarAlloc, 30_4)

    IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( 15_4 )

    PRINT *, 'LEN( charScalarAlloc ) =', LEN( charScalarAlloc )
    PRINT *, "'", charScalarAlloc, "'"

    IF (LEN( charScalarAlloc ) /= 71) CALL zzrc( 16_4 )

    IF (charScalarAlloc /=&
        ('Test with long CHARACTER Var. and ' //&
            '"Test with a long CHARACTER Variable"')) CALL zzrc( 17_4 )

END PROGRAM deferredLenDummyArg01


SUBROUTINE AssignSub(charScalarArg, failRCBase)
    CHARACTER(:), ALLOCATABLE :: charScalarArg
    INTEGER(4) :: failRCBase

    CHARACTER(29) :: char29Var = 'Test with long CHARACTER Var.'
    CHARACTER(37) :: char37Var = '"Test with a long CHARACTER Variable"'


    charScalarArg = char29Var // " and " // char37Var

    IF (.NOT. ALLOCATED( charScalarArg )) CALL zzrc( (failRCBase + 1_4) )

    PRINT *, 'LEN( charScalarArg ) =', LEN( charScalarArg )
    PRINT *, "'", charScalarArg, "'"

END SUBROUTINE AssignSub

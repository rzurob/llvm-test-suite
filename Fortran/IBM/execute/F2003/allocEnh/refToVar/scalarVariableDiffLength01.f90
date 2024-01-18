!*  ===================================================================
!*
!*                               expr Contain References to variable
!*
!*  DATE                       : October 24, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of CHARACTER
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

PROGRAM scalarVariableDiffLength01

    INTEGER(4) :: rc = 10_4
    INTEGER, PARAMETER :: N = 7

    CHARACTER(15) :: title
    CHARACTER(N), ALLOCATABLE :: charScalarAlloc


    charScalarAlloc = 'aBcDeFg'
    CALL DumpCheck(' Initialization', rc)


    rc = rc + 10_4
    charScalarAlloc = charScalarAlloc
    CALL DumpCheck('Self Assignment', rc)

    rc = rc + 10_4
    charScalarAlloc = charScalarAlloc( 2:6 )
    CALL DumpCheck('     Sub-String', rc)

    rc = rc + 10_4
    charScalarAlloc = charScalarAlloc( :4 ) // charScalarAlloc
    CALL DumpCheck('  Concatenation', rc)


    !
    !  Bc D e B cD  =>  B Bc e cD D   (1,2 3 4 5 6,7  =>  3 1,2 4 6,7 5)
    !  B B cec D D  =>  D B cec D B   (1 2 3,4,5 6 7  =>  6 1 3,4,5 7 2)
    !   D BcecD B   =>   B BcecD D    (1 2,3,4,5,6 7  =>  7 2,3,4,5,6 1)
    !
    DO i = 1, (N / 2)
        charScalarAlloc =&
            charScalarAlloc( ((N / 2) + 1 + i):((N / 2) + 1 + i) ) //&
            charScalarAlloc( :((N / 2) - i) ) //&
            charScalarAlloc( ((N / 2) + 2 - i):((N / 2) + i) ) //&
            charScalarAlloc( ((N / 2) + i + 2): ) //&
            charScalarAlloc( ((N / 2) + 1 - i):((N / 2) + 1 - i) ) //&
            charScalarAlloc

        WRITE(title, '(A11," (",I1,")")') 'Invert', i

        rc = rc + 10_4
        CALL DumpCheck(title, rc)
    END DO


    CONTAINS


        SUBROUTINE DumpCheck(test, failRC)
            CHARACTER(*) :: test
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charScalarAlloc )) CALL zzrc( failRC )

            PRINT *, test, ":  LEN = ", LEN( charScalarAlloc ),&
                        "(", N, "), '", charScalarAlloc, "'"

            IF (LEN( charScalarAlloc ) /= N) CALL zzrc( (failRC + 1_4) )

        END SUBROUTINE DumpCheck

END PROGRAM scalarVariableDiffLength01

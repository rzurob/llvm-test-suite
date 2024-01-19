!*  ===================================================================
!*
!*                               and/or expr Contain References to variable
!*
!*  DATE                       : October 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE Array of INTEGER
!*                               Intrinsic Type, and
!*  SECONDARY FUNCTIONS TESTED : expr is a Zero Length Array Section of
!*                               variable
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mModule

    INTEGER(2), ALLOCATABLE :: intArrAlloc( :,:,: )

END MODULE mModule


PROGRAM exprIsZeroLengthArraySection01
    USE mModule

    INTERFACE
        SUBROUTINE CheckDump(sizes, testNum, title, failRC)
            USE mModule
            INTEGER :: sizes( 3 )
            INTEGER(4) :: testNum
            CHARACTER(*) :: title
            INTEGER(4) :: failRC
        END SUBROUTINE CheckDump
    END INTERFACE


    INTEGER(4) :: i
    INTEGER :: sizes( 3,3 ) = RESHAPE((/ 0,4,4, 4,0,4, 4,4,0 /), (/ 3,3 /))


    DO i = 1, 3
        CALL Allocate( )
        CALL CheckDump((/ 4,4,4 /), i, 'a)', (((i - 1_4) * 40) + 10_4))

        CALL Assign( )
        CALL CheckDump(sizes( :,i ), i, 'b)', (((i - 1_4) * 40) + 30_4))

        DEALLOCATE( intArrAlloc )
    END DO


    CONTAINS

        SUBROUTINE Assign( )

            IF (i == 1_4) THEN
                intArrAlloc = intArrAlloc( ::-1,:,: )

            ELSE IF (i == 2_4) THEN
                intArrAlloc = intArrAlloc( :,::-1,: )

            ELSE
                intArrAlloc = intArrAlloc( :,:,::-1 )
            END IF

        END SUBROUTINE Assign

END PROGRAM exprIsZeroLengthArraySection01


SUBROUTINE Allocate( )
    USE mModule

    ALLOCATE(intArrAlloc( 4,4,4 ),&
        SOURCE=RESHAPE((/ (((INT(((i * 16) + (j * 4) + k), 2),&
                k = 0, 3), j = 0, 3), i = 0, 3) /), (/ 4,4,4 /)))

END SUBROUTINE Allocate


SUBROUTINE CheckDump(sizes, testNum, title, failRC)
    USE mModule

    INTEGER :: sizes( 3 )
    INTEGER(4) :: testNum
    CHARACTER(*) :: title
    INTEGER(4) :: failRC

    INTEGER(4) :: i


    IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )

    PRINT *
    PRINT *, testNum, title, KIND( intArrAlloc )

    IF (KIND( intArrAlloc ) /= 2) CALL zzrc( (failRC + 1_4) )


    DO i = 1_4, 3_4
        PRINT *, '    ', LBOUND(intArrAlloc, i),&
                         UBOUND(intArrAlloc, i), SIZE(intArrAlloc, i)

        IF (LBOUND(intArrAlloc, i) /= 1) CALL zzrc( (failRC + 1_4 + i) )
        IF (UBOUND(intArrAlloc, i) /= sizes( i ))&
                            CALL zzrc( (failRC + 4_4 + i) )

        IF (SIZE(intArrAlloc, i) /= sizes( i )) CALL zzrc( (failRC + 7_4 + i) )
    END DO

END SUBROUTINE CheckDump

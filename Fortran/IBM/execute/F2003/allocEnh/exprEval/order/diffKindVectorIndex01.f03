!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : October 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type INTEGER
!*  SECONDARY FUNCTIONS TESTED : expr references elements of variable
!*                               (Indexed using a Vector Subscript), and
!*                               will have a different Shape/Length Type
!*                               Parameter Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  diffKindVectorIndex01:   Default
!*  diffKindVectorIndex01a:  -qintsize=2
!*  diffKindVectorIndex01b:  -qintsize=4
!*  diffKindVectorIndex01c:  -qintsize=8
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

PROGRAM diffKindVectorIndex01

	INTEGER :: defaultKind
    INTEGER, PARAMETER :: defaultIntKind = KIND( defaultKind )

    INTEGER, ALLOCATABLE :: arrIntAlloc( :,: )


    arrIntAlloc =&
        RESHAPE((/ ((((i * 10) + j), j = 0, 9), i = 0, 9) /), (/ 10,10 /))

    IF (KIND( arrIntAlloc ) /= defaultIntKind) ERROR STOP 10_4
    CALL DumpArray( 'Default Kind (10 x 10):' )


    arrIntAlloc =&
        INT(arrIntAlloc( (/ 2,3,4,5,6,7,8,9 /),(/ 2,3,4,5,6,7,8,9 /) ), 2)

    IF (KIND( arrIntAlloc ) /= defaultIntKind) ERROR STOP 20_4
    CALL DumpArray( 'Kind 2 (8 x 8):' )


    arrIntAlloc =&
        INT(arrIntAlloc(&
                (/ 1,1,(i, i = 1, 8),8,8 /),(/ 1,1,(i, i = 1, 8),8,8 /) ), 4)

    IF (KIND( arrIntAlloc ) /= defaultIntKind) ERROR STOP 30_4
    CALL DumpArray( 'Kind 4 (12 x 12):' )


    arrIntAlloc =&
        INT(arrIntAlloc( (/ (i, i = 5, 8) /),(/ (i, i = 5, 8) /) ), 8)

    IF (KIND( arrIntAlloc ) /= defaultIntKind) ERROR STOP 40_4
    CALL DumpArray( 'Kind 8 (4 x 4):' )


    CONTAINS

        SUBROUTINE DumpArray( title )
            CHARACTER(*) :: title

            PRINT *
            PRINT *, title

            DO i = 1, SIZE(arrIntAlloc, 2)
                PRINT *, arrIntAlloc( :,i )
            END DO

        END SUBROUTINE DumpArray

END PROGRAM diffKindVectorIndex01

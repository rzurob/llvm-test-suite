!*  ===================================================================
!*
!*  DATE                       : October 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Array of CHARACTER with
!*                               Deferred Length
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Shape Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 8
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is a pointer, the value of expr is assigned to the target
!*  of variable.
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

PROGRAM ptrToVarArrAsExpr06

    CHARACTER(:), POINTER :: charArrPtr( : )
    CHARACTER(:), ALLOCATABLE, TARGET :: charArrAlloc( : )


    charArrAlloc = [ ('1234567890', i = 1, 10) ]
    CALL Dump(10, 10, 'Initial', 10_4)


    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( 2:9 )
    CALL Dump(8, 10, 'Reshape', 20_4)

    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( 2:7 )( 2:9 )
    CALL Dump(6, 8, 'Reshape', 30_4)

    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( : )( 2:7 )
    CALL Dump(6, 6, 'Resize', 40_4)

    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( [ 1,2,3, (i, i = 1, 6), 4, 5, 6 ] )
    CALL Dump(12, 6, 'Reshape', 50_4)

    charArrPtr => charArrAlloc
    charArrAlloc =&
        '012' // charArrPtr( [ 1,2, (i, i = 1, 12), 5, 6 ] ) // '9ab'
    CALL Dump(16, 12, 'Reshape', 60_4)

    charArrPtr => charArrAlloc
    charArrAlloc = 'ef' // charArrPtr( : ) // 'cd'
    CALL Dump(16, 16, 'Reshape', 70_4)

    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( [ (i, i = 1, 16), (i, i = 1, 16) ] )( 3:14 )
    CALL Dump(32, 12, 'Re-Size/Shape', 80_4)

    charArrPtr => charArrAlloc
    charArrAlloc = charArrPtr( 5:28 ) // 'cdef' // charArrPtr( 5:28 )( 1:8 )
    CALL Dump(24, 24, 'Re-Size/Shape', 90_4)


    CONTAINS


        SUBROUTINE Dump(size1, length, title, failRC)
            INTEGER :: size1
            INTEGER :: length1
            CHARACTER(*) :: title
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT '(A14," (SIZE=",I2,", LEN=",I2,"):")', title, size1, length
            PRINT *, SIZE( charArrAlloc ), "(", size1, ")",&
                        LEN( charArrAlloc ), "(", length, ")"

            DO i = 1, SIZE( charArrAlloc )
                PRINT *, i, "'", charArrAlloc( i ), "'"
            END DO


            IF (SIZE( charArrAlloc ) /= size1) CALL zzrc( (failRC + 1_4) )
            IF (LEN( charArrAlloc ) /= length) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE Dump

END PROGRAM ptrToVarArrAsExpr06

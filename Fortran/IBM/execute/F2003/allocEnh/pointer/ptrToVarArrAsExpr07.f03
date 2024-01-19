!*  ===================================================================
!*
!*  DATE                       : October 24, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr uses a POINTER that has a TARGET of
!*                               variable and the value of expr has a different
!*                               Shape Result
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

MODULE mBase

    TYPE :: tBase
        INTEGER, ALLOCATABLE :: b
    END TYPE tBase

END MODULE mBase

MODULE mDerived
    USE mBase

    TYPE :: tDerived
        INTEGER :: d
        TYPE(tBase),ALLOCATABLE :: tB( : )
    END TYPE tDerived

END MODULE mDerived

PROGRAM ptrToVarArrAsExpr07
    USE mDerived

    TYPE(tDerived), POINTER :: dArrPtr( : )
    TYPE(tDerived), ALLOCATABLE, TARGET :: dArr( : )


    ALLOCATE(dArr( 10 ), SOURCE=[ (tDerived((11 - i),NULL()), i = 1, 10) ])

    DO i = 1, 10
        ALLOCATE( dArr( i )%tB( dArr( i )%d ) )

        DO j = 1, dArr( i )%d
            ALLOCATE(dArr( i )%tB( j )%b, SOURCE=(((i - 1) * 10) + j))
        END DO
    END DO

    CALL Dump(10, 'Init', 10_4)


    dArrPtr => dArr
    dArr = dArr( 2:9 )
    CALL Dump(8, 'Assign (8)', 20_4)

    dArrPtr => dArr
    dArr = dArr( [ 8,7,(i, i = 1, 8),2,1 ] )
    CALL Dump(12, 'Assign (12)', 30_4)


    CONTAINS

        SUBROUTINE Dump(sizeD, title, failRC)
            INTEGER :: sizeD
            CHARACTER(*) :: title
            INTEGER(4) :: failRC

            INTEGER(4) :: i


            IF (.NOT. ALLOCATED( dArr )) CALL zzrc( failRC )

            PRINT *
            PRINT *, title
            PRINT *, 'S( dArr ) =', SIZE( dArr ), '(', sizeD, ')'


            DO i = 1, SIZE( dArr )
                IF (.NOT. ALLOCATED( dArr( i )%tB )) CALL zzrc( (failRC + 1_4) )

                DO j = 1, SIZE( dArr( i )%tB )
                    IF (.NOT. ALLOCATED( dArr( i )%tB( j )%b )) THEN
                        PRINT *, i, j
                        CALL zzrc( (failRC + 2_4) )
                    END IF

                    IF (dArr( i )%tB( j )%b /=&
                        (((10 - dArr( i )%d) * 10) + j)) THEN
                        PRINT *, i, j, dArr( i )%d, dArr( i )%tB( j )%b
                        CALL zzrc( (failRC + 3_4) )
                    END IF
                END DO


                PRINT *, 'S( dArr(', i, ')%tB() ) =',&
                         SIZE( dArr( i )%tB ), '(', dArr( i )%d, ')',&
                         (dArr( i )%tB( j )%b, j = 1, SIZE( dArr( i )%tB ))


                IF (SIZE( dArr( i )%tB ) /= dArr( i )%d)&
                                    CALL zzrc( (failRC + 4_4) )
            END DO

            IF (SIZE( dArr ) /= sizeD) CALL zzrc( (failRC + 5_4) )

        END SUBROUTINE Dump

END PROGRAM ptrToVarArrAsExpr07

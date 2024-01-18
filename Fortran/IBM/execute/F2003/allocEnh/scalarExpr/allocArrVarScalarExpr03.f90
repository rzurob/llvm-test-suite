!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*                               with an ALLOCATABLE Component
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of the same Derived
!*                               Type
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocArrVarScalarExpr03

    TYPE :: tType1
        INTEGER :: i
    END TYPE tType1

    TYPE :: tType2
        INTEGER :: i
        TYPE(tType1), ALLOCATABLE :: t1
    END TYPE tType2


    TYPE(tType2) :: t2Scalar
    TYPE(tType2), ALLOCATABLE :: t2( :,: )


    t2Scalar%i = -1
    ALLOCATE(t2Scalar%t1, SOURCE=tType1(-2))


    ALLOCATE( t2( 10,10 ) )
    DO i = 1, 10
        DO j = 1, 10
            t2( j,i )%i = 1
            ALLOCATE(t2( j,i )%t1, SOURCE=tType1(2))
        END DO
    END DO

    CALL DumpIt(10, 10, 1, 2, 10_4)


    t2 = t2Scalar
    CALL DumpIt(10, 10, -1, -2, 110_4)


    CONTAINS


        SUBROUTINE DumpIt(s1, s2, x, y, failRC)
            INTEGER :: s1
            INTEGER :: s2
            INTEGER :: x
            INTEGER :: y
            INTEGER(4) :: failRC

            INTEGER(4) :: i

            IF (.NOT. ALLOCATED( t2 )) CALL zzrc( failRC )


            PRINT *
            PRINT *, SIZE(t2, 1), "(", s1, ")", SIZE(t2, 2), "(", s2, ")"
            DO j = 1, SIZE(t2, 1)
                DO i = 1, SIZE(t2, 2)
                    IF (.NOT. ALLOCATED( t2( j,i )%t1 )) THEN
                        CALL zzrc( (failRC + 10_4 + i) )
                    END IF
                END DO


                PRINT *, (t2( j,i )%i, i = 1, SIZE(t2, 2)), '  ',&
                                (t2( j,i )%t1%i, i = 1, SIZE(t2, 2))


                DO i = 1, SIZE(t2, 2)
                    IF (t2( j,i )%i /= x) THEN
                        CALL zzrc( (failRC + 20_4 + i) )

                    ELSE IF (t2( j,i )%t1%i /= y) THEN
                        CALL zzrc( (failRC + 30_4 + i) )
                    END IF
                END DO
            END DO


            IF (SIZE(t2, 1) /= s1) CALL zzrc( (failRC + 6_4) )
            IF (SIZE(t2, 2) /= s2) CALL zzrc( (failRC + 7_4) )

        END SUBROUTINE DumpIt

END PROGRAM allocArrVarScalarExpr03

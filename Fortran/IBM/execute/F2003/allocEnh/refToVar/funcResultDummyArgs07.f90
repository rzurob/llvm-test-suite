!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : funcResultDummyArgs07 - variable and/or
!*                               expr Contain References to variable
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 23, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived
!*                               Type and expr calls a User Defined FUNCTION
!*                               that has variable as the Actual Argument.
!*  SECONDARY FUNCTIONS TESTED : Elements of the corresponding Dummy Argument
!*                               are Assigned to the FUNCTION's Result.
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
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mType1

    TYPE tType1
        REAL, ALLOCATABLE :: r
    END TYPE tType1

END MODULE mType1


MODULE mType2
    USE mType1


    TYPE tType2
        INTEGER :: i
        TYPE(tType1), ALLOCATABLE :: t1
    END TYPE tType2

    TYPE(tType2), ALLOCATABLE :: t2( : )

END MODULE mType2


PROGRAM funcResultDummyArgs07
    USE mType2


    INTEGER(4) :: rc = 10_4
    INTEGER :: s = 12


    ALLOCATE(t2( s ), SOURCE=(/ (tType2(i,NULL()), i = 1, s) /))

    DO i = 1, s
        IF ((MOD(i, 3) == 2)  .OR.  (MOD(i, 3) == 0)) THEN
            ALLOCATE( t2( i )%t1 )

            IF (MOD(i, 3) == 0) THEN
                ALLOCATE(t2( i )%t1%r, SOURCE=REAL( i ))
            END IF
        END IF
    END DO


    CALL Check(s, rc)
    DO WHILE (SIZE( t2 ) > 1)
        t2 = t2F( t2 )

        s = s / 2
        rc = rc + 10_4

        CALL Check(s, rc)
    END DO


    CONTAINS


        FUNCTION t2F( t2Arg )
            TYPE(tType2), ALLOCATABLE :: t2Arg( : )

            TYPE(tType2), ALLOCATABLE :: t2F( : )


            t2F = (/ (t2Arg( i ), i = 2, SIZE( t2Arg ), 2) /)

        END FUNCTION t2F


        SUBROUTINE Check(size1, failRC)
            INTEGER :: size1
            INTEGER(4) :: failRC

            IF (.NOT. ALLOCATED( t2 ))  CALL zzrc( failRC )

            PRINT *
            PRINT *, SIZE( t2 ), "(", size1, ")"

            DO i = 1, SIZE( t2 )
                IF ( ALLOCATED( t2( i )%t1 ) ) THEN
                    IF ( ALLOCATED( t2( i )%t1%r ) ) THEN
                        PRINT '(I2," - ",F4.1)', t2( i )%i, t2( i )%t1%r

                    ELSE
                        PRINT 10, t2( i )%i, i, "%r"
                    END IF

                ELSE
                    PRINT 10, t2( i )%i, i, ""
10                  FORMAT(I2," - t2(",I2,")%t1",A2)
                END IF
            END DO

            IF (SIZE( t2 ) /= size1)    CALL zzrc( (failRC + 1_4) )

        END SUBROUTINE Check

END PROGRAM funcResultDummyArgs07

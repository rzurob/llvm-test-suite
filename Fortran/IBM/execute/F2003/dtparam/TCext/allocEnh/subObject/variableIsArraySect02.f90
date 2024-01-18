! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/subObject/variableIsArraySect02.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*                               Subobject
!*
!*  DATE                       : November  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section (2 Dimensions) of a multiple
!*                               Dimension Allocated ALLOCATABLE Array
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
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
!*  When variable is a subobject, the assignment does not affect the
!*  definition status or value of other parts of the object. For example,
!*  if variable is an array section, the assignment does not affect the
!*  definition status or value of the elements of the array not specified
!*  by the array section.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM variableIsArraySect02

    TYPE :: tType(N1,K1)    ! (20,4)
        INTEGER, KIND         :: K1
        INTEGER, LEN          :: N1
        REAL(K1), ALLOCATABLE :: tC( : )
    END TYPE tType

    INTEGER(4) :: m

    TYPE(tType(20,4)) :: tT2( 2,2 )
    TYPE(tType(:,4)), ALLOCATABLE :: tTA( :,:,:,: )


    tT2 = RESHAPE(&
        [ (tType(20,4)([ (-j, j = i, (i + 1)) ]), i = 1, 8, 2) ], [ 2,2 ])

    m = 5_4
    DO i = 1, SIZE(tT2, 2)
        DO j = 1, SIZE(tT2, 2)
            m = m + 2_4
            CALL DumpTT(tT2( j,i ), 2, m)
        END DO
    END DO


    tTA = RESHAPE(&
        [ (tType(20,4)([ (j, j = i, (i + 2)) ]), i = 1, 48, 3) ], [ 2,2,2,2 ])
    CALL DumpTTA(RESHAPE([ 3,3,3,3 ], [ 2,2 ]), 10_4)


    tTA( 2,2,:,: ) = tT2
    CALL DumpTTA(RESHAPE([ 3,3,3,2 ], [ 2,2 ]), 50_4)


    CONTAINS


        SUBROUTINE DumpTTA(s, rc)
            INTEGER :: s( 2,2 )
            INTEGER(4) :: rc

            INTEGER(4) :: m


            IF (.NOT. ALLOCATED( tTA )) CALL zzrc( rc )


            PRINT *
            PRINT *, SIZE(tTA, 1), SIZE(tTA, 2), SIZE(tTA, 3), SIZE(tTA, 4)


            m = 0_4
            DO i = 1, 4
                m = m + 1_4
                IF (SIZE(tTA, i) /= 2) CALL zzrc( (rc + m) )
            END DO


            DO i = 1, SIZE(tTA, 4)
                DO j = 1, SIZE(tTA, 3)
                    DO k = 1, SIZE(tTA, 2)
                        DO l = 1, SIZE(tTA, 1)
                            m = m + 2_4
                            CALL DumpTT(tTA( l,k,j,i ), s( l,k ), (rc + m))
                        END DO
                    END DO
                END DO
            END DO

        END SUBROUTINE DumpTTA


        SUBROUTINE DumpTT(t, s, rc)
            TYPE(tType(*,4)) :: t
            INTEGER :: s
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( t%tC )) CALL zzrc( rc )

            PRINT *, rc, SIZE( t%tC ), s, (t%tC( n ), n = 1, SIZE( t%tC ))

            IF (SIZE( t%tC ) /= s) CALL zzrc( (rc + 1_4) )

        END SUBROUTINE DumpTT

END PROGRAM variableIsArraySect02

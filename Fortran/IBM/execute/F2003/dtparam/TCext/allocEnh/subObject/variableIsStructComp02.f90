! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/subObject/variableIsStructComp02.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*                               Subobject
!*
!*  DATE                       : November  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               Derived Type (with a Structure Component
!*                               that is an ALLOCATABLE Array)
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
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

PROGRAM variableIsStructComp02

    TYPE :: tType(N1,K1)    ! (20,4)
        INTEGER, KIND            :: K1
        INTEGER, LEN             :: N1
        INTEGER(K1), ALLOCATABLE :: aA( :,: )
    END TYPE tType

    TYPE(tType(20,4)) :: tT2x2
    TYPE(tType(20,4)), TARGET :: tT5x5
    TYPE(tType(20,4)) :: tT10x10

    TYPE(tType(:,4)), POINTER :: tPtr
    TYPE(tType(:,4)), ALLOCATABLE, TARGET :: tT


    tT2x2 = tType(20,4)(RESHAPE([ ((i ** i), i = 1, 4) ], [ 2,2 ]))
    tT5x5 = tType(20,4)(RESHAPE([ (-i, i = 1, 25) ], [ 5,5 ]))
    tT10x10 = tType(20,4)(RESHAPE([ (i, i = 1, 100) ], [ 10,10 ]))


    tT = tT5x5
    tPtr => tT
    IF (.NOT. ALLOCATED( tT )) CALL zzrc( 10_4 )
    CALL Check(tT5x5, 10_4)

    tT = tT2x2
    IF (.NOT. ALLOCATED( tT )) CALL zzrc( 20_4 )
    CALL Check(tT2x2, 20_4)

    tT = tT10x10
    IF (.NOT. ALLOCATED( tT )) CALL zzrc( 30_4 )
    CALL Check(tT10x10, 30_4)

    tT = tType(20,4)(RESHAPE(tT%aA, [ 20,5 ]))
    IF (.NOT. ALLOCATED( tT )) CALL zzrc( 40_4 )
    CALL Check(tType(20,4)(RESHAPE(tT10x10%aA, [ 20,5 ])), 40_4)


    tT5x5 = tT2x2
    tPtr => tT5x5
    CALL Check(tT2x2, 50_4)

    tT5x5 = tT10x10
    CALL Check(tT10x10, 60_4)

    tT5x5 = tT
    CALL Check(tT, 70_4)

    tT5x5 = tType(20,4)(RESHAPE(tT5x5%aA, [ 4,25 ]))
    CALL Check(tType(20,4)(RESHAPE(tT10x10%aA, [ 4,25 ])), 80_4)


    CONTAINS


        SUBROUTINE Check(t, rc)
            TYPE(tType(*,4)) :: t
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( tPtr%aA )) CALL zzrc( (rc + 1_4) )


            PRINT *
            PRINT *, rc, SIZE(tPtr%aA, 1), SIZE(t%aA, 1),&
                            SIZE(tPtr%aA, 2), SIZE(t%aA, 2)

            DO i = 1, SIZE(tPtr%aA, 1)
                PRINT *, (tPtr%aA( i,j ), j = 1, SIZE(tPtr%aA, 2))
            END DO


            IF (SIZE(tPtr%aA, 1) /= SIZE(t%aA, 1)) CALL zzrc( (rc + 2_4) )
            IF (SIZE(tPtr%aA, 2) /= SIZE(t%aA, 2)) CALL zzrc( (rc + 3_4) )

            IF ( ANY( (tPtr%aA /= t%aA) ) ) CALL zzrc( (rc + 4_4) )

        END SUBROUTINE Check

END PROGRAM variableIsStructComp02

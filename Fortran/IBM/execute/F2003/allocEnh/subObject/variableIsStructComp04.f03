!*  ===================================================================
!*
!*                               Subobject
!*
!*  DATE                       : November 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is the
!*                               Array Section of an Allocated ALLOCATABLE
!*                               Structure Component of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
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

PROGRAM variableIsStructComp04

    TYPE :: tType
        INTEGER :: s
        INTEGER, ALLOCATABLE :: iA( : )
    END TYPE tType

    TYPE(tType) :: tT1
    TYPE(tType) :: tT3
    TYPE(tType) :: tT5

    TYPE(tType), POINTER :: tTPtr

    TYPE(tType), TARGET :: tTCheck
    TYPE(tType), ALLOCATABLE, TARGET :: tTA


    tT1 = tType(1,[ 99 ])
    tT3 = tType(3,[ (i, i = 1, 3) ])
    tT5 = tType(5,[ (-i, i = 5, 1, -1) ])

    tTCheck = tT5
    tTPtr => tTCheck
    CALL Check(tT5, 5, 10_4)


    tTA = tT5
    tTPtr => tTA
    IF (.NOT. ALLOCATED( tTA )) ERROR STOP 20_4
    CALL Check(tTCheck, 5, 20_4)


    DO i = 2, 4
        tTCheck%iA( i ) = tT3%iA( (i - 1) )
    END DO

    tTA%iA( 2:4 ) = tT3%iA
    IF (.NOT. ALLOCATED( tTA )) ERROR STOP 30_4
    CALL Check(tTCheck, 5, 30_4)


    DO i = 2, 4
        tTCheck%iA( i ) = tT5%iA( (i - 1) )
    END DO

    tTA%iA( 2:4 ) = tT5%iA
    IF (.NOT. ALLOCATED( tTA )) ERROR STOP 40_4
    CALL Check(tTCheck, 5, 40_4)


!    tTCheck%iA( 2 ) = tT1%iA( 1 )
!
!    tTA%iA( 2:4 ) = tT1%iA         ! Possible missed conformance check
!    IF (.NOT. ALLOCATED( tTA )) ERROR STOP 50_4
!    CALL Check(tTCheck, 5, 50_4)   ! Could also be a Defect for AllocEnh
!    Returns:   50 5 5 5 -5 99 0 1532690432 -1

    CONTAINS

        SUBROUTINE Check(t, s, rc)
            TYPE(tType) :: t
            INTEGER :: s
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( tTPtr%iA )) CALL zzrc( (rc + 1_4) )

            PRINT *, rc, SIZE( tTPtr%iA ), tTPtr%s, s, tTPtr%iA

            IF (tTPtr%s /= s)   CALL zzrc( (rc + 2_4) )
            IF (tTPtr%s /= t%s) CALL zzrc( (rc + 3_4) )

            IF (SIZE( t%iA ) /= s)      CALL zzrc( (rc + 4_4) )
            IF (SIZE( tTPtr%iA ) /= s)  CALL zzrc( (rc + 5_4) )

            IF ( ANY( (tTPtr%iA /= t%iA) ) ) CALL zzrc( (rc + 6_4) )

        END SUBROUTINE Check

END PROGRAM variableIsStructComp04

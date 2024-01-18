!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : variableIsStructComp03 - variable is a
!*                               Subobject
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is a
!*                               Derived Type (with a Structure Component
!*                               that is an ALLOCATABLE Array of another
!*                               Derived Type)
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 5
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
!*  When variable is a subobject, the assignment does not affect the
!*  definition status or value of other parts of the object. For example,
!*  if variable is an array section, the assignment does not affect the
!*  definition status or value of the elements of the array not specified
!*  by the array section.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM variableIsStructComp03

    TYPE :: t1
        COMPLEX, ALLOCATABLE :: c
    END TYPE t1

    TYPE :: t2
        TYPE(t1), ALLOCATABLE :: a( : )
    END TYPE t2

    TYPE(t2) :: t2_2
    TYPE(t2) :: t2_10

    TYPE(t2), TARGET :: t2_5

    TYPE(t2), POINTER :: t2Ptr
    TYPE(t2), ALLOCATABLE, TARGET :: t2a


    t2_2 = t2([ (t1((REAL( i ) / 10.)), i = 1, 2) ])
    t2_5 = t2([ (t1( -i ), i = 1, 5) ])
    t2_10 = t2([ (t1( i ), i = 0, 9) ])


    t2a = t2_5
    t2Ptr => t2a
    IF (.NOT. ALLOCATED( t2a )) CALL zzrc( 10_4 )
    CALL Check(t2_5, 10_4)

    t2a = t2_2
    IF (.NOT. ALLOCATED( t2a )) CALL zzrc( 40_4 )
    CALL Check(t2_2, 40_4)

    t2a = t2_10
    IF (.NOT. ALLOCATED( t2a )) CALL zzrc( 70_4 )
    CALL Check(t2_10, 70_4)


    t2_5 = t2_2
    t2Ptr => t2_5
    CALL Check(t2_2, 100_4)

    t2_5 = t2_10
    CALL Check(t2_10, 130_4)


    CONTAINS


        SUBROUTINE Check(t, rc)
            TYPE(t2) :: t
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( t2Ptr%a )) CALL zzrc( (rc + 1_4) )

            PRINT *
            PRINT *, rc, SIZE( t2Ptr%a ), SIZE( t%a )


            IF (SIZE( t2Ptr%a ) /= SIZE( t%a )) CALL zzrc( (rc + 2_4) )

            DO i = 1, SIZE( t2Ptr%a )
                IF (.NOT. ALLOCATED( t2Ptr%a( i )%c ))&
                    CALL zzrc( (rc + INT(i, 4) + 4_4) )


                PRINT *, t2Ptr%a( i )%c


                IF (t2Ptr%a( i )%c /= t%a( i )%c)&
                    CALL zzrc( (rc + INT(i, 4) + 14_4) )
            END DO

        END SUBROUTINE Check

END PROGRAM variableIsStructComp03

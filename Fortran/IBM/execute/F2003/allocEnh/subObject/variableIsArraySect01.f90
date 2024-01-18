!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : variableIsArraySect01 - variable is a
!*                               Subobject
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Array Section (a single Dimension) of a
!*                               multiple Dimension Allocated ALLOCATABLE
!*                               Array
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
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
!*  When variable is a subobject, the assignment does not affect the
!*  definition status or value of other parts of the object. For example,
!*  if variable is an array section, the assignment does not affect the
!*  definition status or value of the elements of the array not specified
!*  by the array section.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM variableIsArraySect01

    TYPE :: tB
        INTEGER, ALLOCATABLE :: b
    END TYPE tB

    TYPE, EXTENDS(tB) :: tD
        INTEGER :: d
    END TYPE tD

    TYPE(tD), ALLOCATABLE :: d1( : )
    TYPE(tD), ALLOCATABLE :: d3( :,:,: )


    d1 = [ (tD(-9,-9), i = 1, 3) ]
    d3 = RESHAPE([ (tD(i,i), i = 26, 0, -1) ], [ 3,3,3 ])

    IF (.NOT. ALLOCATED( d1 )) CALL zzrc( 10_4 ) 

    PRINT *, SIZE( d1 ), (d1( i )%b, i = 1, SIZE( d1 ))
    PRINT *, ' ', d1( : )%d

    IF (SIZE( d1 ) /= 3) CALL zzrc( 11_4 ) 

    DO i = 1, SIZE(d3, 1)
        IF (d1( i )%b /= d1( i )%d) CALL zzrc( (11_4 + INT(i, 4)) )
        IF (d1( i )%b /= -9)        CALL zzrc( (15_4 + INT(i, 4)) )
    END DO


    CALL Dump( 20_4 )

    d3( 1,:,1 ) = d1
    CALL Dump( 60_4 )


    CONTAINS


        SUBROUTINE Dump( rc )
            INTEGER(4) :: rc

            INTEGER(4) :: m


            IF (.NOT. ALLOCATED( d3 )) CALL zzrc( rc ) 


            PRINT *
            PRINT *, rc, SIZE(d3, 1), SIZE(d3, 2), SIZE(d3, 3)


            IF (SIZE(d3, 1) /= 3) CALL zzrc( (rc + 1_4) )
            IF (SIZE(d3, 2) /= 3) CALL zzrc( (rc + 2_4) )
            IF (SIZE(d3, 3) /= 3) CALL zzrc( (rc + 3_4) )


            DO i = 1, SIZE(d3, 3)
                DO j = 1, SIZE(d3, 2)
                    PRINT *, j, i, (d3( k,j,i )%b, k = 1, SIZE(d3, 1))
                    PRINT *, '   ', d3( :,j,i )%d

                    DO k = 1, SIZE(d3, 1)
                        l = ((i - 1) * 9) + ((j - 1) * 3) + k
                        m = rc + 10_4 + INT(l, 4)

                        IF (d3( k,j,i )%b /= d3( k,j,i )%d) CALL zzrc( m )
                    END DO
                END DO
            END DO

        END SUBROUTINE Dump

END PROGRAM variableIsArraySect01

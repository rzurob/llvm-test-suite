!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : derivedTypePolyComp03 - variable is a
!*                               Subobject
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*                               with a Polymorphic Component
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

PROGRAM derivedTypePolyComp03

    TYPE :: tType
        INTEGER :: s
        INTEGER :: et
        CLASS(*), ALLOCATABLE :: p( : )
    END TYPE tType

    CHARACTER(5) :: char5( 5 ) =&
        [ 'aBcDe', 'FgHiJ', 'kLmNo', 'PqRsT', 'uVwXy' ]

    TYPE(tType), ALLOCATABLE :: t


    t = tType(10,1,[ (INT(i, 2), i = 1, 10) ])
    CALL Check(10, 10_4)

    t = tType(10,2,[ (REAL(i, 8), i = 1, 10) ])
    CALL Check(10, 20_4)

    t = tType(5,3,[ (CMPLX(i, 4), i = 1, 5) ])
    CALL Check(5, 30_4)

    t = tType(5,4,[ (LOGICAL(.TRUE., 4), i = 1, 5) ])
    CALL Check(5, 40_4)

    t = tType(5,5,[ (char5( i ), i = 1, 5) ])
    CALL Check(5, 50_4)


    CONTAINS


        SUBROUTINE Check(s, rc)
            INTEGER :: s
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( t ))   CALL zzrc( rc )
            IF (.NOT. ALLOCATED( t%p )) CALL zzrc( (rc + 1_4) )

            PRINT *
            PRINT *, rc, SIZE( t%p ), t%s, s
            IF (t%s /= s)           CALL zzrc( (rc + 2_4) )
            IF (SIZE( t%p ) /= s)   CALL zzrc( (rc + 3_4) )


            SELECT TYPE (x => t%p)
                TYPE IS ( INTEGER(2) )
                    PRINT *, t%et, x
                    IF (t%et /= 1) CALL zzrc( (rc + 4_4) )

                TYPE IS ( REAL(8) )
                    PRINT '(I2,10F5.1)', t%et, x
                    IF (t%et /= 2) CALL zzrc( (rc + 5_4) )

                TYPE IS ( COMPLEX(4) )
                    PRINT '(I2,5(" (",F3.1,",",F3.1,")"))', t%et, x
                    IF (t%et /= 3) CALL zzrc( (rc + 6_4) )

                TYPE IS ( LOGICAL(4) )
                    PRINT *, t%et, x
                    IF (t%et /= 4) CALL zzrc( (rc + 7_4) )

                TYPE IS ( CHARACTER(*) )
                    PRINT '(I2,5(" (",A5,")"))', t%et, x
                    IF (t%et /= 5) CALL zzrc( (rc + 8_4) )

                CLASS DEFAULT
                    PRINT *, 'Unknown Type: t%p'
                    CALL zzrc( (rc + 9_4) )
            END SELECT

        END SUBROUTINE Check

END PROGRAM derivedTypePolyComp03

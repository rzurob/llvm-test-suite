!*  ===================================================================
!*
!*                               Subobject
!*
!*  DATE                       : November 10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is the
!*                               Allocated ALLOCATABLE of Derived Type with
!*                               a Polymorphic Component
!*  SECONDARY FUNCTIONS TESTED : and expr is of the same type as variable
!*
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

PROGRAM derivedTypePolyComp01

    TYPE :: tType
        CLASS(*), ALLOCATABLE :: p
    END TYPE tType

    TYPE(tType) :: tI2
    TYPE(tType), ALLOCATABLE :: t


    t = tType(5_1)
    CALL Check(1, 10_4)

    t = tType(10_4)
    CALL Check(3, 20_4)

    tI2 = tType(15_2)
    t = tI2
    CALL Check(2, 30_4)

    t = tType(20_8)
    CALL Check(4, 40_4)


    t = tType((25.0_16 , 30.0_16))
    CALL Check(5, 50_4)

    t = tType('abcdefghi')
    CALL Check(6, 60_4)

    t = tType(t)
    CALL Check(7, 70_4)



    CONTAINS


        SUBROUTINE Check(et, rc)
            INTEGER :: et
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( t )) CALL zzrc( rc )
            IF (.NOT. ALLOCATED( t%p )) CALL zzrc( (rc + 1_4) )

            SELECT TYPE (x => t%p)
                TYPE IS ( INTEGER(1) )
                    PRINT *, rc, et, x
                    IF (et /= 1) CALL zzrc( (rc + 2_4) )

                TYPE IS ( INTEGER(2) )
                    PRINT *, rc, et, x
                    IF (et /= 2) CALL zzrc( (rc + 3_4) )

                TYPE IS ( INTEGER(4) )
                    PRINT *, rc, et, x
                    IF (et /= 3) CALL zzrc( (rc + 4_4) )

                TYPE IS ( INTEGER(8) )
                    PRINT *, rc, et, x
                    IF (et /= 4) CALL zzrc( (rc + 5_4) )

                TYPE IS ( COMPLEX(16) )
                    PRINT '(I3,I2," (",F4.1,",",F4.1,")")', rc, et, x
                    IF (et /= 5) CALL zzrc( (rc + 6_4) )

                TYPE IS ( CHARACTER(*) )
                    PRINT *, rc, et, x
                    IF (et /= 6) CALL zzrc( (rc + 7_4) )

                TYPE IS ( tType )
                    SELECT TYPE (y => x%p)
                        TYPE IS ( CHARACTER(*) )
                            PRINT *, rc, et, y
                            IF (et /= 7) CALL zzrc( (rc + 8_4) )

                        CLASS DEFAULT
                            PRINT *, 'Unknown Type: x%p'
                            CALL zzrc( (rc + 8_4) )
                    END SELECT

                CLASS DEFAULT
                    PRINT *, 'Unknown Type: t%p'
                    CALL zzrc( (rc + 9_4) )
            END SELECT

        END SUBROUTINE Check

END PROGRAM derivedTypePolyComp01

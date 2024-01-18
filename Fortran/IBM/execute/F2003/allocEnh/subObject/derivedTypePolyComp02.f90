!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : derivedTypePolyComp02 - variable is a
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
!*  NUMBER OF TESTS CONDITIONS : 2
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

PROGRAM derivedTypePolyComp02

    TYPE :: tType
        INTEGER :: et
        CLASS(*), ALLOCATABLE :: p
    END TYPE tType

    TYPE(tType) :: tB( 7 )
    TYPE(tType) :: tC( 6 )
    TYPE(tType), ALLOCATABLE :: tA( : )


    tB = [ tType(1,5.0_4), tType(3,10.0_16), tType(2,15.0_8), tType(4,20_2),&
            tType(5,(25.0_16 , 30.0_16)), tType(6,'A1B2C3D4E'), tType(7,3) ]

    tB( 7 ) = tType(7,tB( 3 ))

    tC = [ tType(6,'abc'), tType(4,123_2), tType(2,7.8_8),&
           tType(7,7), tType(7,3), tType(5,(12.3_16 , 45.6_16)) ]

    tC( 4 ) = tType(7,tC( 3 ))
    tC( 5 ) = tType(7,tB( 3 ))


    tA = tB
    CALL Check(7, 10_4)

    tA = tC
    CALL Check(6, 110_4)


    CONTAINS


        SUBROUTINE Check(s, rc)
            INTEGER :: s
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( tA )) CALL zzrc( rc )

            PRINT *
            PRINT *, rc, SIZE( tA ), s
            IF (SIZE( tA ) /= s) CALL zzrc( (rc + 1_4) )

            DO i = 1, SIZE( tA )
                rc = rc + 10_4
                CALL CheckP(i, rc)
            END DO

        END SUBROUTINE Check


        SUBROUTINE CheckP(i, rc)
            INTEGER :: i
            INTEGER(4) :: rc


            IF (.NOT. ALLOCATED( tA( i )%p )) CALL zzrc( (rc + 1_4) )

            SELECT TYPE (x => tA( i )%p)
                TYPE IS ( REAL(4) )
                    PRINT '(I3,I2,I2,F4.1)', rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 1) CALL zzrc( (rc + 2_4) )

                TYPE IS ( REAL(8) )
                    PRINT '(I3,I2,I2,F5.1)', rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 2) CALL zzrc( (rc + 3_4) )

                TYPE IS ( REAL(16) )
                    PRINT '(I3,I2,I2,F5.1)', rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 3) CALL zzrc( (rc + 4_4) )

                TYPE IS ( INTEGER(2) )
                    PRINT *, rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 4) CALL zzrc( (rc + 5_4) )

                TYPE IS ( COMPLEX(16) )
                    PRINT '(I3,I2,I2," (",F4.1,",",F4.1,")")',&
                                            rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 5) CALL zzrc( (rc + 6_4) )

                TYPE IS ( CHARACTER(*) )
                    PRINT *, rc, i, tA( i )%et, x
                    IF (tA( i )%et /= 6) CALL zzrc( (rc + 7_4) )

                TYPE IS ( tType )
                    SELECT TYPE (y => x%p)
                        TYPE IS ( REAL(8) )
                            PRINT '(I3,I2,I2,F5.1)', rc, i, tA( i )%et, y
                            IF (tA( i )%et /= 7) CALL zzrc( (rc + 8_4) )

                        CLASS DEFAULT
                            PRINT *, 'Unknown Type: x%p'
                            CALL zzrc( (rc + 8_4) )
                    END SELECT

                CLASS DEFAULT
                    PRINT *, 'Unknown Type: tA(', i, ')%p'
                    CALL zzrc( (rc + 9_4) )
            END SELECT

        END SUBROUTINE CheckP

END PROGRAM derivedTypePolyComp02

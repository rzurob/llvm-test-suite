!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : deferedVariableSelector01
!*  TEST CASE TITLE            : variable selector with Defered Length
!*                               Parameter(s)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 22, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable is has a Defered Length
!*                               Parameter
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above):
!*  o selector has Assumed/Defered Length Type Parameters
!*    - a variable declared to be of a Derived Type that requires a Defered
!*      Length Type Parameter
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mud
    IMPLICIT NONE

    TYPE dirt(k)
        INTEGER(8), LEN :: k
        INTEGER :: dirtyArray(k)
    END TYPE dirt

    TYPE, EXTENDS(dirt) :: gravel(l)
        INTEGER(8), LEN :: l
        INTEGER :: gravelyArray(l)
    END TYPE gravel

    TYPE, EXTENDS(gravel) :: crushed(x)
        INTEGER(2), KIND :: x
        REAL(x) :: crushedArray( k,l )
    END TYPE crushed

END MODULE mud


PROGRAM deferedVariableSelector01
    USE mud
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE SandTrap(quickSand, sandPit, sandBox)
            USE mud
            IMPLICIT NONE

            TYPE(gravel(:,:)), POINTER  :: quickSand
            INTEGER :: sandPit( 3 )
            INTEGER :: sandBox( 2 )
        END SUBROUTINE SandTrap
    END INTERFACE

    INTEGER, PARAMETER :: pitted( 3 ) = [ 1, 2, 3 ]
    INTEGER, PARAMETER :: boxed( 2 ) = [ -2, -1 ]

    TYPE(dirt(5)), TARGET :: compost = dirt(5)([ 99, 98, 97, 96, 95 ])
    TYPE(gravel(3,2)), TARGET :: sand = gravel(3,2)(pitted,boxed)

    TYPE(crushed(2,1,4)), TARGET :: particulate =&
        crushed(2,1,4)([ -99, -98 ],[ 111 ],RESHAPE([ 3.14_4, 1.5_4 ],[ 2,1 ]))

    TYPE(dirt(:)), POINTER :: dirtyPtr => NULL( )
    CLASS(dirt(:)), POINTER :: soil => NULL( )

    TYPE(gravel(:,:)), POINTER :: pointSander => NULL( )


    dirtyPtr => compost
    ASSOCIATE(muck => dirtyPtr)
        IF ( ANY(SHAPE( muck%dirtyArray )&
                    /= SHAPE( compost%dirtyArray )) ) THEN
            STOP 10

        ELSE IF ( ANY(muck%dirtyArray /= compost%dirtyArray) ) THEN
            STOP 11
        END IF
    END ASSOCIATE


    pointSander => sand
    CALL SandTrap(pointSander, pitted, boxed)


    soil => compost
    CALL Quary(30_4, .FALSE., .FALSE.)

    soil => sand
    CALL Quary(40_4, .TRUE., .FALSE.)

    soil => particulate
    CALL Quary(50_4, .TRUE., .TRUE.)


    CONTAINS


    SUBROUTINE Quary(rc, extendedDT, doublyExtendedDT)
            INTEGER(4) :: rc
            LOGICAL :: extendedDT
            LOGICAL :: doublyExtendedDT

                ASSOCIATE(earth => soil)
                    SELECT TYPE ( earth )
                        CLASS IS (dirt(*))
                            IF ( ANY( SHAPE( earth%dirtyArray ) /=&
                                        SHAPE( soil%dirtyArray )) ) THEN
                                CALL zzrc( rc )

                            ELSE IF ( ANY(earth%dirtyArray /=&
                                            soil%dirtyArray) ) THEN
                                CALL zzrc( (rc + 1_4) )
                            END IF

                        CLASS DEFAULT
                            CALL zzrc( (rc + 2_4) )
                    END SELECT

                    SELECT TYPE ( earth )
                        CLASS IS (gravel(*,*))
                            SELECT TYPE ( soil )
                                CLASS IS (gravel(*,*))
                                    IF ( ANY( SHAPE( earth%gravelyArray ) /=&
                                            SHAPE( soil%gravelyArray )) ) THEN
                                        CALL zzrc( (rc + 3_4) )

                                    ELSE IF ( ANY(earth%gravelyArray /=&
                                                    soil%gravelyArray) ) THEN
                                        CALL zzrc( (rc + 4_4) )
                                    END IF

                                CLASS DEFAULT
                                    IF ( extendedDT ) THEN
                                        CALL zzrc( (rc + 5_4) )
                                    END IF
                            END SELECT

                        CLASS DEFAULT
                            IF ( extendedDT ) THEN
                                CALL zzrc( (rc + 6_4) )
                            END IF
                    END SELECT

                    SELECT TYPE ( earth )
                        TYPE IS (crushed(*,*,4))
                            IF ( ANY(SHAPE( earth%crushedArray ) /=&
                                    SHAPE( particulate%crushedArray )) ) THEN
                                CALL zzrc( (rc + 7_4) )

                            ELSE IF ( ANY(earth%crushedArray /=&
                                            particulate%crushedArray) ) THEN
                                CALL zzrc( (rc + 8_4) )
                            END IF

                        CLASS DEFAULT
                            IF ( doublyExtendedDT ) THEN
                                CALL zzrc( (rc + 9_4) )
                            END IF
                    END SELECT
                END ASSOCIATE

        END SUBROUTINE Quary

END PROGRAM deferedVariableSelector01


SUBROUTINE SandTrap(quickSand, pit, box)
    USE mud
    IMPLICIT NONE

    TYPE(gravel(:,:)), POINTER :: quickSand
    INTEGER :: pit( 3 )
    INTEGER :: box( 2 )

    ASSOCIATE(dunes => quickSand)
        IF ( ANY(SHAPE( dunes%dirtyArray ) /= SHAPE( pit )) ) THEN
            STOP 20

        ELSE IF ( ANY(dunes%dirtyArray /= pit) ) THEN
            STOP 21

        ELSE IF ( ANY(SHAPE( dunes%gravelyArray ) /= SHAPE( box )) ) THEN
            STOP 22

        ELSE IF ( ANY(dunes%gravelyArray /= box) ) THEN
            STOP 23
        END IF
    END ASSOCIATE

END SUBROUTINE SandTrap

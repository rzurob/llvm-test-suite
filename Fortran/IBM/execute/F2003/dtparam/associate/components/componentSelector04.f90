!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : componentSelector04
!*  TEST CASE TITLE            : selector is a Component of a Derived Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September  8, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component is an ALLOCATABLE Polymorphic
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Is an ALLOCATABLE Polymorphic
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tBaseMod
    IMPLICIT NONE

    TYPE tBase(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: compType
        CLASS(*), ALLOCATABLE :: compPolyBase( : )
    END TYPE tBase

END MODULE tBaseMod


MODULE tDerivedMod
    USE tBaseMod

    IMPLICIT NONE

    TYPE, EXTENDS(tBase) :: tDerived(k)
        INTEGER, KIND :: k

        LOGICAL(k) :: compAlloc = .FALSE.
        CLASS(*), ALLOCATABLE :: compPolyExt( : )
    END TYPE tDerived

END MODULE tDerivedMod


PROGRAM componentSelector04
    USE tDerivedMod

    IMPLICIT NONE

    INTERFACE
        RECURSIVE SUBROUTINE Dump(scalar, i, rc)
            USE tDerivedMod
            IMPLICIT NONE
            CLASS(tBase(*)) :: scalar
            INTEGER(4) :: i
            INTEGER(4) :: rc
        END SUBROUTINE Dump
    END INTERFACE


    INTEGER(4) :: i = 1_4

    CHARACTER(9) :: intStr = '  Integer'
    CHARACTER(9) :: realStr = '     Real'
    CHARACTER(9) :: compStr = '  Complex'
    CHARACTER(9) :: logStr = '  Logical'
    CHARACTER(9) :: charStr = 'Character'

    TYPE(tBase(9)) :: intrinsicPoly( 5 )
    CLASS(tBase(:)), ALLOCATABLE :: derivedPoly


    intrinsicPoly = [   tBase(9)(intStr,[ (INT(i, 2), i = 1, 9) ]),&
                        tBase(9)(realStr,[ (REAL(i, 16), i = 1, 9) ]),&
                        tBase(9)(compStr,[ (CMPLX(i, i, 8), i = 1, 9) ]),&
                        tBase(9)(logStr,[ ((MOD(i,2) == 0), i = 1, 9) ]),&
                        tBase(9)(charStr,[ (ACHAR((64 + i)), i = 1, 9) ])   ]


    DO i = 1_4, 5_4
        CALL Dump(intrinsicPoly( i ), i, (i * 10_4))
    END DO


    ALLOCATE(tDerived(7,2) :: derivedPoly)

    SELECT TYPE (derivedPoly)
        TYPE IS (tDerived(*,2))
            derivedPoly =&
                tDerived(7,2)('Derived',[ intrinsicPoly( 1 ) ],&
                                    .TRUE.,[ intrinsicPoly( 5 ) ])

        CLASS DEFAULT
            CALL zzrc( 80_4 )
    END SELECT

    CALL Dump(derivedPoly, 6_4, 100_4)

END PROGRAM componentSelector04


RECURSIVE SUBROUTINE Dump(scalar, i, rc)
    USE tDerivedMod

    IMPLICIT NONE

    CLASS(tBase(*)) :: scalar
    INTEGER(4) :: i
    INTEGER(4) :: rc

    CHARACTER(255) :: fmt


    SELECT TYPE ( scalar )
        TYPE IS (tBase(*))
            WRITE(fmt,*) '(I3,") ",A', scalar%l, ',I3, I2)'
            WRITE(6,fmt) i, scalar%compType,scalar%l,SIZE( scalar%compPolyBase )

            ASSOCIATE(polyBase => scalar%compPolyBase)

                SELECT TYPE (polyBase => scalar%compPolyBase)
                    TYPE IS (INTEGER(2))
                        fmt = BuildFmt('I1', (scalar%l - 2), 'I1', 'I2')
                        WRITE(6,fmt) KIND(polyBase), polyBase

                    TYPE IS (REAL(16))
                        fmt = BuildFmt('I2', (scalar%l - 2), 'F3.1', 'F4.1')
                        WRITE(6,fmt) KIND(polyBase), polyBase

                    TYPE IS (COMPLEX(8))
                        fmt = BuildFmt('I1', (scalar%l - 2),&
                                        '"(",F3.1,",",F3.1,")"',&
                                        '" (",F3.1,",",F3.1,")"')
                        WRITE(6,fmt) KIND(polyBase), polyBase

                    TYPE IS (LOGICAL(4))
                        fmt = BuildFmt('I1', (scalar%l - 2), 'L1', 'L2')
                        WRITE(6,fmt) KIND(polyBase), polyBase

                    TYPE IS (CHARACTER(*))
                        fmt = BuildFmt('I1,",",I1', (scalar%l - 2), 'A1', 'A2')
                        WRITE(6,fmt) KIND(polyBase), LEN(polyBase), polyBase

                    CLASS DEFAULT
                        CALL zzrc( (rc + 1_4) )
                END SELECT

            END ASSOCIATE

        TYPE IS (tDerived(*,2))
            WRITE(fmt,*) '(I3,") ",A', scalar%l, ',I3,I2,L2)'
            WRITE(6,fmt) i, scalar%compType, scalar%l,scalar%k, scalar%compAlloc

            IF (.NOT. ALLOCATED( scalar%compPolyBase )) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (.NOT. ALLOCATED( scalar%compPolyExt )) THEN
                CALL zzrc( (rc + 3_4) )
            END IF

            ASSOCIATE(polyBase => scalar%compPolyBase,&
                        polyExt => scalar%compPolyExt)

                IF (SIZE( polyBase ) /= 1) THEN
                    CALL zzrc( (rc + 4_4) )
                END IF

                SELECT TYPE ( polyBase )
                    TYPE IS (tBase(*))
                        PRINT *
                        CALL Dump(polyBase( 1 ), (i + 1_4), (rc + 10_4))

                    CLASS DEFAULT
                        CALL zzrc( (rc + 5_4) )
                END SELECT

                IF (SIZE( polyExt ) /= 1) THEN
                    CALL zzrc( (rc + 6_4) )
                END IF

                SELECT TYPE ( polyExt )
                    TYPE IS (tBase(*))
                        CALL Dump(polyExt( 1 ), (i + 2_4), (rc + 20_4))

                    CLASS DEFAULT
                        CALL zzrc( (rc + 7_4) )
                END SELECT

            END ASSOCIATE

        CLASS DEFAULT
            CALL zzrc( (rc + 8_4) )
    END SELECT

    PRINT *

    CONTAINS

        CHARACTER(255) FUNCTION BuildFmt(kindFmt, numElements,&
                                            firstElementFmt, elementFmt)
            CHARACTER(*) :: kindFmt
            INTEGER :: numElements
            CHARACTER(*) :: firstElementFmt
            CHARACTER(*) :: elementFmt

            WRITE(BuildFmt,*) '("    polyBase(",', kindFmt, ',") => (",',&
                                firstElementFmt, ',",",', numElements, '(',&
                                    elementFmt, ',","),', elementFmt, ',")")'

        END FUNCTION BuildFmt

END SUBROUTINE Dump

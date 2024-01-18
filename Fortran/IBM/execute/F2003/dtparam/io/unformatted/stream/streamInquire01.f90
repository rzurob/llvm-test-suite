!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : streamInquire01
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 28, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : IOLENGTH= Specifier for the INQUIRE
!*                               Statement
!*  SECONDARY FUNCTIONS TESTED : Derived/Extended Derived Types (with
!*                               Parameters) and PRIVATE Components
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To determine the Unformatted I/O Length for a Derived Type and an
!*  Extended Derived Type from within Type-Bound Procedures.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE privateBaseMod
    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1), PRIVATE :: a( l1 )

        CONTAINS

            GENERIC :: DumpB => DumpB16, DumpB8, DumpB4

            PROCEDURE, PASS :: DumpB16
            PROCEDURE, PASS :: DumpB8
            PROCEDURE, PASS :: DumpB4

    END TYPE base

    CONTAINS

        INTEGER FUNCTION DumpB16( this )
            CLASS(base(*,16)) :: this

            SELECT TYPE (this)
                TYPE IS (base(*,16))
                    INQUIRE( IOLENGTH=DumpB16 ) this

                CLASS DEFAULT
                    DumpB16 = -2
            END SELECT

        END FUNCTION DumpB16

        INTEGER FUNCTION DumpB8( this )
            CLASS(base(*,8)) :: this

            SELECT TYPE (this)
                TYPE IS (base(*,8))
                    INQUIRE( IOLENGTH=DumpB8 ) this

                CLASS DEFAULT
                    DumpB8 = -2
            END SELECT

        END FUNCTION DumpB8

        INTEGER FUNCTION DumpB4( this )
            CLASS(base(*,4)) :: this

            SELECT TYPE (this)
                TYPE IS (base(*,4))
                    INQUIRE( IOLENGTH=DumpB4 ) this

                CLASS DEFAULT
                    DumpB4 = -2
            END SELECT

        END FUNCTION DumpB4

END MODULE privateBaseMod


MODULE privateExtMod
    USE privateBaseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext

        CONTAINS

            GENERIC :: DumpE => DumpE16, DumpE8, DumpE4

            PROCEDURE, PASS :: DumpE16
            PROCEDURE, PASS :: DumpE8
            PROCEDURE, PASS :: DumpE4

    END TYPE ext

    CONTAINS

        INTEGER FUNCTION DumpE16( this )
            CLASS(ext(*,16)) :: this

            INTEGER :: lenThis

            DumpE16 = this%base%DumpB( )

            select type(this)
                type is(ext(*,16))
                    INQUIRE( IOLENGTH=lenThis ) this
            end select
            IF (DumpE16 /= lenThis) THEN
                PRINT *, 'DumpE16() =', DumpE16, ', (', lenThis, ')'
                DumpE16 = -1
            END IF

        END FUNCTION DumpE16

        INTEGER FUNCTION DumpE8( this )
            CLASS(ext(*,8)) :: this

            INTEGER :: lenThis

            DumpE8 = this%base%DumpB( )

            select type(this)
                type is(ext(*,8))
                    INQUIRE( IOLENGTH=lenThis ) this
            end select

            IF (DumpE8 /= lenThis) THEN
                PRINT *, 'DumpE8() =', DumpE8, ', (', lenThis, ')'
                DumpE8 = -1
            END IF

        END FUNCTION DumpE8

        INTEGER FUNCTION DumpE4( this )
            CLASS(ext(*,4)) :: this

            INTEGER :: lenThis

            DumpE4 = this%base%DumpB( )

            select type(this)
                type is(ext(*,4))
                    INQUIRE( IOLENGTH=lenThis ) this
            end select

            IF (DumpE4 /= lenThis) THEN
                PRINT *, 'DumpE4() =', DumpE4, ', (', lenThis, ')'
                DumpE4 = -1
            END IF

        END FUNCTION DumpE4

END MODULE privateExtMod


PROGRAM streamInquire01
    USE privateExtMod

    IMPLICIT NONE

    INTEGER :: iostat

    INTEGER :: b4Len, e4Len
    INTEGER :: b8Len, e8Len
    INTEGER :: b16Len, e16Len

    TYPE(base(5,4))  :: b4
    TYPE(ext(5,4))   :: e4

    TYPE(base(5,8))  :: b8
    TYPE(ext(5,8))   :: e8

    TYPE(base(5,16)) :: b16
    TYPE(ext(5,16))  :: e16


    b4Len = b4%DumpB( )
    PRINT *, 'b4%DumpB( ) =', b4Len
    IF (b4Len /= 20) STOP 10

    e4Len = e4%DumpE( )
    PRINT *, 'e4%DumpE( ) =', e4Len
    IF (e4Len /= 20) STOP 11

    PRINT *


    b8Len = b8%DumpB( )
    PRINT *, 'b8%DumpB( ) =', b8Len
    IF (b8Len /= 40) STOP 20

    e8Len = e8%DumpE( )
    PRINT *, 'e8%DumpE( ) =', e8Len
    IF (e8Len /= 40) STOP 21

    PRINT *


    b16Len = b16%DumpB( )
    PRINT *, 'b16%DumpB( ) =', b16Len
    IF (b16Len /= 80) STOP 30

    e16Len = e16%DumpE( )
    PRINT *, 'e16%DumpE( ) =', e16Len
    IF (e16Len /= 80) STOP 31

    PRINT *

END PROGRAM streamInquire01

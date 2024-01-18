!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : streamWrite05
!*
!*  DATE                       : October 27, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : STREAM File Output of a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Base Derived Type has a PRIVATE Component
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITEs of a Derived Type and Extended Derived
!*  Type (both with a PRIVATE Component) to a Stream file, then READ the
!*  data written, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE privateBaseMod
    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1), PRIVATE :: a( l1 )

        CONTAINS

            GENERIC :: SetA => SetA16, SetA8, SetA4
            GENERIC :: DumpB => DumpB16, DumpB8, DumpB4

            PROCEDURE, PASS :: SetA16
            PROCEDURE, PASS :: SetA8
            PROCEDURE, PASS :: SetA4

            PROCEDURE, PASS :: DumpB16
            PROCEDURE, PASS :: DumpB8
            PROCEDURE, PASS :: DumpB4

    END TYPE base

    CONTAINS

        SUBROUTINE SetA16(this, a)
            CLASS(base(*,16)) :: this
            REAL(16) :: a(:)

            IF (SIZE( a ) < this%l1) THEN
                this%a( 1:SIZE( a ) ) = a

            ELSE
                this%a = a( 1:this%l1 )
            END IF

        END SUBROUTINE SetA16

        INTEGER FUNCTION DumpB16(this, u)
            CLASS(base(*,16)) :: this
            INTEGER :: u

            CHARACTER(255) :: iomsg = 'Unknown Type'


            SELECT TYPE (this)
                TYPE IS (base(*,16))
                    PRINT *, 'DumpB16(): this%a = [', this%a, ']'
                    WRITE(u, IOSTAT=DumpB16, IOMSG=iomsg) this

                CLASS DEFAULT
                    DumpB16 = -1
            END SELECT

            IF (DumpB16 /= 0) THEN
                PRINT *, "DumpB16():  WRITE(", u, ",", DumpB16, ") ", iomsg
            END IF

        END FUNCTION DumpB16

        SUBROUTINE SetA8(this, a)
            CLASS(base(*,8)) :: this
            REAL(8) :: a(:)

            IF (SIZE( a ) < this%l1) THEN
                this%a( 1:SIZE( a ) ) = a

            ELSE
                this%a = a( 1:this%l1 )
            END IF

        END SUBROUTINE SetA8

        INTEGER FUNCTION DumpB8(this, u)
            CLASS(base(*,8)) :: this
            INTEGER :: u

            CHARACTER(255) :: iomsg = 'Unknown Type'


            SELECT TYPE (this)
                TYPE IS (base(*,8))
                    PRINT *, 'DumpB8(): this%a = [', this%a, ']'
                    WRITE(u, IOSTAT=DumpB8, IOMSG=iomsg) this

                CLASS DEFAULT
                    DumpB8 = -1
            END SELECT

            IF (DumpB8 /= 0) THEN
                PRINT *, "DumpB8():  WRITE(", u, ",", DumpB8, ") ", iomsg
            END IF

        END FUNCTION DumpB8

        SUBROUTINE SetA4(this, a)
            CLASS(base(*,4)) :: this
            REAL(4) :: a(:)

            IF (SIZE( a ) < this%l1) THEN
                this%a( 1:SIZE( a ) ) = a

            ELSE
                this%a = a( 1:this%l1 )
            END IF

        END SUBROUTINE SetA4

        INTEGER FUNCTION DumpB4(this, u)
            CLASS(base(*,4)) :: this
            INTEGER :: u

            CHARACTER(255) :: iomsg = 'Unknown Type'


            SELECT TYPE (this)
                TYPE IS (base(*,4))
                    PRINT *, 'DumpB4(): this%a = [', this%a, ']'
                    WRITE(u, IOSTAT=DumpB4, IOMSG=iomsg) this

                CLASS DEFAULT
                    DumpB4 = -1
            END SELECT

            IF (DumpB4 /= 0) THEN
                PRINT *, "DumpB4():  WRITE(", u, ",", DumpB4, ") ", iomsg
            END IF

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

        INTEGER FUNCTION DumpE16(this, u)
            CLASS(ext(*,16)) :: this
            INTEGER :: u

            DumpE16 = this%base%DumpB( u )

        END FUNCTION DumpE16

        INTEGER FUNCTION DumpE8(this, u)
            CLASS(ext(*,8)) :: this
            INTEGER :: u

            DumpE8 = this%base%DumpB( u )

        END FUNCTION DumpE8

        INTEGER FUNCTION DumpE4(this, u)
            CLASS(ext(*,4)) :: this
            INTEGER :: u

            DumpE4 = this%base%DumpB( u )

        END FUNCTION DumpE4

END MODULE privateExtMod


PROGRAM streamWrite05
    USE privateExtMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: iostat

    REAL(4) :: r4b( 5 ), r4e( 5 )
    REAL(8) :: r8b( 5 ), r8e( 5 )
    REAL(16) :: r16b( 5 ), r16e( 5 )

    REAL(16) :: array( 5 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 5) ]

    CHARACTER(255) :: iomsg

    TYPE(base(5,4))  :: b4
    TYPE(ext(5,4))   :: e4

    TYPE(base(5,8))  :: b8
    TYPE(ext(5,8))   :: e8

    TYPE(base(5,16)) :: b16
    TYPE(ext(5,16))  :: e16
    logical(4), external :: precision_r4, precision_r8,precision_r6


    OPEN(42, ACCESS='stream', ACTION='write', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        STOP 10
    END IF


    CALL b4%SetA( REAL(array, 4) )
    iostat = b4%DumpB( 42 )
    IF (iostat /= 0) STOP 20

    CALL e4%SetA( REAL(array, 4) )
    iostat = e4%DumpE( 42 )
    IF (iostat /= 0) STOP 21


    CALL b8%SetA( REAL(array, 8) )
    iostat = b8%DumpB( 42 )
    IF (iostat /= 0) STOP 30

    CALL e8%SetA( REAL(array, 8) )
    iostat = e8%DumpE( 42 )
    IF (iostat /= 0) STOP 31


    CALL b16%SetA( array )
    iostat = b16%DumpB( 42 )
    IF (iostat /= 0) STOP 40

    CALL e16%SetA( array )
    iostat = e16%DumpE( 42 )
    IF (iostat /= 0) STOP 41


    CLOSE(42, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 50
    END IF


    OPEN(42, ACCESS='stream', ACTION='read', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        STOP 60
    END IF


    READ(42, IOSTAT=iostat, IOMSG=iomsg) r4b, r4e, r8b, r8e, r16b, r16e
    IF (iostat /= 0) THEN
        PRINT *, "READ(", iostat, ") ", iomsg
        STOP 70
    END IF


    CLOSE(42, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        STOP 80
    END IF


    do i = 1, 5
        IF ( .not. precision_r4(r4b(i), real(array(i),4)))   STOP 100
        IF ( .not. precision_r4(r4b(i), r4e(i)))             STOP 101

        IF ( .not. precision_r8(r8b(i), real(array(i),8)))   STOP 110
        IF ( .not. precision_r8(r8b(i), r8e(i)))             STOP 111

        IF ( .not. precision_r6(r16b(i), array(i)))          STOP 120
        IF ( .not. precision_r6(r16b(i), r16e(i)))           STOP 121
    end do

END PROGRAM streamWrite05

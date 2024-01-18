!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialReadWrite04
!*
!*  DATE                       : October  8, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File I/O for a base Derived Type
!*                               and a Container Derived Type
!*  SECONDARY FUNCTIONS TESTED : I/O is performed via Type-Bound Procedures
!*                               (on a per-element basis)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform Unformatted Derived Type I/O with Type-Bound Procedures
!*  where one of the Dervied Types is a Container for the other Derived
!*  Type.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE r
    IMPLICIT NONE

    TYPE tR(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        REAL(k) :: rA( l )

        CONTAINS

            PROCEDURE, PASS :: ReadTR8
            PROCEDURE, PASS :: WriteTR8

    END TYPE tR

    CONTAINS

        INTEGER FUNCTION ReadTR8(dTR, u, rc)
            CLASS(tr(*,8)) :: dTR
            INTEGER :: u
            INTEGER(4) :: rc

            CHARACTER(255) :: iomsg

            SELECT TYPE ( dTR )
                TYPE IS (tR(*,8))
                    READ(u, IOSTAT=ReadTR8, IOMSG=iomsg) dTR
                    IF (ReadTR8 /= 0) THEN
                        PRINT *, "ReadTR8(): READ(", ReadTR8, ") ", iomsg
                    END IF

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION ReadTR8

        INTEGER FUNCTION WriteTR8(dTR, u, rc)
            CLASS(tr(*,8)) :: dTR
            INTEGER :: u
            INTEGER(4) :: rc

            CHARACTER(255) :: iomsg

            SELECT TYPE ( dTR )
                TYPE IS (tR(*,8))
                    WRITE(u, IOSTAT=WriteTR8, IOMSG=iomsg) dTR
                    IF (WriteTR8 /= 0) THEN
                        PRINT *, "WriteTR8(): WRITE(", WriteTR8, ") ", iomsg
                    END IF

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION WriteTR8

END MODULE r


MODULE c
    USE r

    IMPLICIT NONE

    TYPE tC(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        COMPLEX(k) :: cA( l )
        TYPE(tR((2 * l),k)) :: t

        CONTAINS

            PROCEDURE, PASS :: ReadTC8
            PROCEDURE, PASS :: WriteTC8

    END TYPE tC

    CONTAINS

        INTEGER FUNCTION ReadTC8(dTC, u, rc)
            CLASS(tC(8,*)) :: dTC
            INTEGER :: u
            INTEGER(4) :: rc

            CHARACTER(255) :: iomsg

            SELECT TYPE ( dTC )
                TYPE IS (tC(8,*))
                    READ(u, IOSTAT=ReadTC8, IOMSG=iomsg) dTC
                    IF (ReadTC8 /= 0) THEN
                        PRINT *, "ReadTC8(): READ(", ReadTC8, ") ", iomsg
                    END IF

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION ReadTC8

        INTEGER FUNCTION WriteTC8(dTC, u, rc)
            CLASS(tC(8,*)) :: dTC
            INTEGER :: u
            INTEGER(4) :: rc

            CHARACTER(255) :: iomsg

            SELECT TYPE ( dTC )
                TYPE IS (tC(8,*))
                    WRITE(u, IOSTAT=WriteTC8, IOMSG=iomsg) dTC
                    IF (WriteTC8 /= 0) THEN
                        PRINT *, "WriteTC8(): WRITE(", WriteTC8, ") ", iomsg
                    END IF

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION WriteTC8

END MODULE c


PROGRAM sequentialReadWrite04
    USE c

    IMPLICIT NONE

    INTEGER(4) :: i

    INTEGER :: j
    INTEGER :: k

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(8) :: realChk( 8,5 )
    COMPLEX(8) :: cmplxChk( 4,5 )

    TYPE(tR(8,8)) :: bR( 5 )
    TYPE(tR(8,8)) :: eR( 5 )

    TYPE(tC(8,4)) :: bC( 5 )
    TYPE(tC(8,4)) :: eC( 5 )


    DO i = 1_4, 5_4
        DO j = 1, 8
            realChk( j,i ) = 1.0_8 / ((REAL((i - 1_4), 8) * 5.0_8) + REAL(j, 8))
        END DO

        DO j = 1, 8, 2
            k = ((j - 1) / 2) + 1
            cmplxChk( k,i ) = CMPLX(realChk( j,i ), realChk( (j + 1),i ), 8)
        END DO

        bR( i )%rA = realChk( :,i )

        bC( i )%cA = cmplxChk( :,i )
        bC( i )%t%rA = realChk( :,i )
    END DO


    OPEN(99, ACTION='readwrite', FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 5_4, 1_4, -1_4
        iostat = bR( i )%WriteTR8(99, (19_4 + i))
        IF (iostat /= 0) CALL zzrc( (24_4 + i) )

        iostat = bC( i )%WriteTC8(99, (29_4 + i))
        IF (iostat /= 0) CALL zzrc( (34_4 + i) )
    END DO


    REWIND(99, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        CALL zzrc( 40_4 )
    END IF


    DO i = 1_4, 5_4
        iostat = eR( i )%ReadTR8(99, (49_4 + i))
        IF (iostat /= 0) CALL zzrc( (54_4 + i) )

        iostat = eC( i )%ReadTC8(99, (59_4 + i))
        IF (iostat /= 0) CALL zzrc( (64_4 + i) )
    END DO


    CLOSE(99, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        CALL zzrc( 70_4 )
    END IF


    DO i = 1_4, 5_4
        j = 6_4 - i

        IF ( ANY(eR( i )%rA /= bR( j )%rA) ) CALL zzrc( (100_4 + i) )
        IF ( ANY(eR( i )%rA /= realChk( :,j )) ) CALL zzrc( (110_4 + i) )

        IF ( ANY(eC( i )%cA /= bC( j )%cA) ) CALL zzrc( (120_4 + i) )
        IF ( ANY(eC( i )%cA /= cmplxChk( :,j )) ) CALL zzrc( (130_4 + i) )

        IF ( ANY(eC( i )%t%rA /= bC( j )%t%rA) ) CALL zzrc( (140_4 + i) )
        IF ( ANY(eC( i )%t%rA /= realChk( :,j )) ) CALL zzrc( (150_4 + i) )
    END DO

END PROGRAM sequentialReadWrite04

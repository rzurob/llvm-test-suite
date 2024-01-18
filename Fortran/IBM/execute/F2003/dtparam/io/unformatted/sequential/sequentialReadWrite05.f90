!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : sequentialReadWrite05
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  9, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SEQUENTIAL File I/O for a base Derived Type
!*                               and a Container Derived Type
!*  SECONDARY FUNCTIONS TESTED : I/O is performed via Type-Bound Procedures
!*                               (on the whole array)
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of an array of Derive Type to a
!*  Sequential file, READ the the data written, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012


MODULE r
    IMPLICIT NONE

    TYPE tR(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        REAL(k) :: rA( l )

        CONTAINS

            PROCEDURE, NOPASS :: ReadTR8
            PROCEDURE, NOPASS :: WriteTR8

    END TYPE tR


    CONTAINS

        INTEGER FUNCTION ReadTR8(dTR, u)
            TYPE(tr(*,8)) :: dTR( : )
            INTEGER :: u

            INTEGER :: i
            CHARACTER(255) :: iomsg

            do i = 1, size(dtr)
                READ(u, IOSTAT=ReadTR8, IOMSG=iomsg) dTR( i )
                IF (ReadTR8 /= 0) THEN
                    PRINT *, "ReadTR8(): READ(", ReadTR8, ") ", iomsg
                    stop 111
                END IF
            end do

        END FUNCTION ReadTR8

        INTEGER FUNCTION WriteTR8(dTR, u)
            TYPE(tR(*,8)) :: dTR( : )
            INTEGER :: u

            INTEGER :: i
            CHARACTER(255) :: iomsg

            do i = 1, size(dtr)
                WRITE(u, IOSTAT=WriteTR8, IOMSG=iomsg) dTR( i )
                IF (WriteTR8 /= 0) THEN
                    PRINT *, "WriteTR8(): WRITE(", WriteTR8, ") ", iomsg
                END IF
            end do

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

            PROCEDURE, NOPASS :: ReadTC8
            PROCEDURE, NOPASS :: WriteTC8

    END TYPE tC

    CONTAINS

        INTEGER FUNCTION ReadTC8(dTC, u)
            TYPE(tC(8,*)) :: dTC( : )
            INTEGER :: u

            INTEGER :: i
            CHARACTER(255) :: iomsg

            do i = 1, size(dtc)
                READ(u, IOSTAT=ReadTC8, IOMSG=iomsg) dTC( i )
                IF (ReadTC8 /= 0) THEN
                    PRINT *, "ReadTC8(): READ(", ReadTC8, ") ", iomsg
                END IF
            end do

        END FUNCTION ReadTC8

        INTEGER FUNCTION WriteTC8(dTC, u)
            TYPE(tC(8,*)) :: dTC( : )
            INTEGER :: u

            INTEGER :: i
            CHARACTER(255) :: iomsg

            do i = 1, size(dtc)
                WRITE(u, IOSTAT=WriteTC8, IOMSG=iomsg) dTC( i )
                IF (WriteTC8 /= 0) THEN
                    PRINT *, "WriteTC8(): WRITE(", WriteTC8, ") ", iomsg
                END IF
            end do

        END FUNCTION WriteTC8
END MODULE c


PROGRAM sequentialReadWrite05
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

!        PRINT *, 'realChk(:,', i, ') = (', realChk( :,i ), ')'
!        PRINT *, 'cmplxChk(:,', i, ') = (', cmplxChk( :,i ), ')'

        bR( i )%rA = realChk( :,i )
!        PRINT *, 'bR(', i, ')%rA = (', bR( i )%rA, ')'

        bC( i )%cA = cmplxChk( :,i )
        bC( i )%t%rA = realChk( :,i )

!        PRINT *, 'bC(', i, ')%cA = (', bC( i )%cA, ')'
!        PRINT *, 'bC(', i, ')%t%rA = (', bC( i )%t%rA, ')'
!        PRINT *
    END DO


    OPEN(99, ACTION='readwrite', FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        CALL zzrc( 10_4 )
    END IF


    iostat = bC%WriteTC8(bC, 99)
    IF (iostat /= 0) CALL zzrc( 20_4 )

    iostat = bR%WriteTR8(bR, 99)
    IF (iostat /= 0) CALL zzrc( 30_4 )


    REWIND(99, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "REWIND(", iostat, ") ", iomsg
        CALL zzrc( 40_4 )
    END IF


    iostat = eC%ReadTC8(eC, 99)
    IF (iostat /= 0) CALL zzrc( 50_4 )

    iostat = eR%ReadTR8(eR, 99)
    IF (iostat /= 0) CALL zzrc( 60_4 )


    CLOSE(99, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "CLOSE(", iostat, ") ", iomsg
        CALL zzrc( 70_4 )
    END IF


    DO i = 1_4, 5_4
!        PRINT *, 'eR(', i, ')%rA = (', eR( i )%rA, ')'
        IF ( ANY(eR( i )%rA /= bR( i )%rA) ) CALL zzrc( (100_4 + i) )
        IF ( ANY(eR( i )%rA /= realChk( :,i )) ) CALL zzrc( (110_4 + i) )

!        PRINT *, 'eC(', i, ')%cA = (', eC( i )%cA, ')'
        IF ( ANY(eC( i )%cA /= bC( i )%cA) ) CALL zzrc( (120_4 + i) )
        IF ( ANY(eC( i )%cA /= cmplxChk( :,i )) ) CALL zzrc( (130_4 + i) )

!        PRINT *, 'eC(', i, ')%t%rA = (', eC( i )%t%rA, ')'
        IF ( ANY(eC( i )%t%rA /= bC( i )%t%rA) ) CALL zzrc( (140_4 + i) )
        IF ( ANY(eC( i )%t%rA /= realChk( :,i )) ) CALL zzrc( (150_4 + i) )

!        PRINT *
    END DO

END PROGRAM sequentialReadWrite05

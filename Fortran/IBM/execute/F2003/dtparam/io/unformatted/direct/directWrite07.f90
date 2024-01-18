!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : directWrite07
!*  TEST CASE TITLE            : Unformatted Intrinsic Input/Output (with DTP)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DIRECT File Input for a Derived/Extended
!*                               Derived Types (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are POINTERS ASSOCIATED with
!*                               Targets
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of a Derived Type POINTER and an
!*  Extended Derived Type from a Direct file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE nonDTP

    IMPLICIT NONE

    TYPE tN
        COMPLEX(8) :: cb( 1 )
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        COMPLEX(8) :: ce( 6 )
    END TYPE eN

END MODULE nonDTP


MODULE DTP

    IMPLICIT NONE

    TYPE tP(k1,l1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        COMPLEX(k1) :: cb( (l1 / 3) )
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP
        COMPLEX(k1) :: ce( (l1 * 2) )
    END TYPE eP

END MODULE DTP


PROGRAM directWrite07
    USE nonDTP
    USE DTP

    INTEGER :: i
    INTEGER :: j

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(8), PARAMETER :: ra( 56 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 56) ]
    COMPLEX(8) :: ca( 28 ) = [ (CMPLX(ra( i ),ra( (i + 1) ),8), i = 1, 56, 2) ]

    TYPE(tN) :: baseIn( 7 )
    TYPE(eN) :: extendedIn( 3 )

    TYPE(tP(8,3)), TARGET :: baseTargetOut( 7 )
    TYPE(tP(8,:)), POINTER :: basePtrOut( : )

    TYPE(eP(8,3)), TARGET :: extendedTargetOut( 3 )
    TYPE(eP(8,:)), POINTER :: extendedPtrOut( : )


    baseTargetOut = [ (tP(8,3)(ca( i )), i = 1, 7) ]
    extendedTargetOut =&
        [ (eP(8,3)(ca( i ),ca( (i + 1):(i + 6) )), i = 8, 28, 7) ]


    basePtrOut => baseTargetOut
    extendedPtrOut => extendedTargetOut


    OPEN(41, ACCESS='direct', ACTION='write',&
            RECL=112, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 1, 3
        WRITE(41, REC=(10 - i), IOSTAT=iostat, IOMSG=iomsg) extendedPtrOut( i )
        IF (iostat /= 0) THEN
            PRINT *, 'WRITE(', iostat, ') ', iomsg
            CALL zzrc( (20_4 + INT(i, 4)) )
        END IF
    END DO

    WRITE(41, REC=5, IOSTAT=iostat, IOMSG=iomsg) (basePtrOut( i ), i = 7, 1, -1)
    IF (iostat /= 0) THEN
        PRINT *, 'WRITE(', iostat, ') ', iomsg
        CALL zzrc( 30_4 )
    END IF


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 40_4 )
    END IF


    OPEN(41, ACCESS='direct', ACTION='read',&
            RECL=112, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 50_4 )
    END IF


    READ(41, REC=5, IOSTAT=iostat, IOMSG=iomsg) (baseIn( i ), i = 7, 1, -1)
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 60_4 )
    END IF

    DO i = 3, 1, -1
        READ(41, REC=(10 - i), IOSTAT=iostat, IOMSG=iomsg) extendedIn( i )
        IF (iostat /= 0) THEN
            PRINT *, 'READ(', iostat, ') ', iomsg
            CALL zzrc( (70_4 + INT(i, 4)) )
        END IF
    END DO


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 80_4 )
    END IF


    DO i = 1, 7
        IF ( ANY(baseIn( i )%cb /= ca( i:i )) )&
                                            CALL zzrc( (100_4 + INT(i, 4)) )
    END DO

    DO i = 1, 3
        j = (i * 7) + 1
        IF ( ANY(extendedIn( i )%cb /= ca( j:j )) )&
                                            CALL zzrc( (110_4 + INT(i, 4)) )

        j = j + 1
        IF ( ANY(extendedIn( i )%ce /= ca( j:(j + 5) ) ) )&
                                            CALL zzrc( (120_4 + INT(i, 4)) )
    END DO

END PROGRAM directWrite07

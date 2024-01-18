!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : streamWrite08
!*
!*  DATE                       : November  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : STREAM File Input for a Derived/Extended
!*                               Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : Derived Types are ALLOCATED Polymorphic
!*                               POINTERs
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted WRITE of a Derived Type/Extended Derived Type
!*  to a Stream file, and confirm the contents.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE notDTP

    IMPLICIT NONE

    TYPE tN
        COMPLEX(16) :: c
    END TYPE tN

    TYPE, EXTENDS(tN) :: eN
        REAL(16) :: r( 2 )
    END TYPE eN

END MODULE notDTP


MODULE dtp

    IMPLICIT NONE

    TYPE tP(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        COMPLEX(k1) :: c
    END TYPE tP

    TYPE, EXTENDS(tP) :: eP
        REAL(k1) :: r( l1 )
    END TYPE eP

    CLASS(tP(:,16)), POINTER :: basePtrOut( : )
    CLASS(eP(:,16)), POINTER :: extPtrOut( : )

    CONTAINS

        INTEGER(4) FUNCTION WriteB(u, i, rc)
            INTEGER :: u
            INTEGER :: i
            INTEGER(4) :: rc

            INTEGER :: iostat = -1
            CHARACTER(255) :: iomsg = 'Unknown Type'

            SELECT TYPE (basePtrOut)
                TYPE IS (tP(*,16))
                    WRITE(u, IOSTAT=iostat, IOMSG=iomsg) basePtrOut( i )
                    WriteB = (1_4 + rc)

                TYPE IS (eP(*,16))
                    WRITE(u, IOSTAT=iostat, IOMSG=iomsg) basePtrOut( i )
                    WriteB = (2_4 + rc)

                CLASS DEFAULT
                    WriteB = rc
            END SELECT

            IF (iostat /= 0) THEN
                PRINT *, 'WriteB():  WRITE(', u,&
                         ',', i, ',', iostat, ') ', iomsg

            ELSE
                WriteB = 0_4
            END IF

        END FUNCTION WriteB

        INTEGER(4) FUNCTION WriteE(u, i, rc)
            INTEGER :: u
            INTEGER :: i
            INTEGER(4) :: rc

            INTEGER :: iostat = -1
            CHARACTER(255) :: iomsg = 'Unknown Type'

            SELECT TYPE (extPtrOut)
                TYPE IS (eP(*,16))
                    WRITE(u, IOSTAT=iostat, IOMSG=iomsg) extPtrOut( i )
                    WriteE = (1_4 + rc)

                CLASS DEFAULT
                    WriteE = rc
            END SELECT

            IF (iostat /= 0) THEN
                PRINT *, 'WriteE():  WRITE(', u,&
                         ',', i, ',', iostat, ') ', iomsg

            ELSE
                WriteE = 0_4
            END IF

        END FUNCTION WriteE

END MODULE dtp


PROGRAM streamWrite08
    USE dtp
    USE notDTP

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER(4) :: rc
    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(16), PARAMETER :: ra( 6 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 6) ]
    COMPLEX(16), PARAMETER :: ca( 3 ) =&
                [ (CMPLX(ra( i ), ra( (i + 1) ), 16), i = 1, 6, 2) ]

    TYPE(tN) :: baseIn( 3 )
    TYPE(eN) :: extendedBaseIn( 3 )
    TYPE(eN) :: extendedIn( 3 )


    OPEN(41, ACCESS='stream', ACTION='write', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 10_4 )
    END IF


    ALLOCATE(basePtrOut( 3 ), SOURCE=[ (tP(2,16)(ca( i )), i = 1, 3) ])

    DO i = 1, 3
        rc = WriteB(41, i, (20_4 + (5_4 * INT(i, 4))))
        IF (rc /= 0_4)  CALL zzrc( rc )
    END DO

    DEALLOCATE( basePtrOut )


    ALLOCATE(eP(2,16) :: basePtrOut( 3 ))
    SELECT TYPE (basePtrOut)
        TYPE IS (eP(*,16))
            DO i = 1, 3
                j = ((i - 1) * 2) + 1

                basePtrOut( i )%c = ca( i )
                basePtrOut( i )%r = ra( j:(j + 1) )
            END DO

        CLASS DEFAULT
            CALL zzrc( 40_4 )
    END SELECT

    DO i = 1, 3
        rc = WriteB(41, i, (50_4 + (5_4 * INT(i, 4))))
        IF (rc /= 0_4)  CALL zzrc( rc )
    END DO

    DEALLOCATE( basePtrOut )


    ALLOCATE(eP(2,16) :: extPtrOut( 3 ))
    SELECT TYPE (extPtrOut)
        TYPE IS (eP(*,16))
            DO i = 1, 3
                j = ((i - 1) * 2) + 1

                extPtrOut( i )%c = ca( i )
                extPtrOut( i )%r = ra( j:(j + 1) )
            END DO

        CLASS DEFAULT
            CALL zzrc( 70_4 )
    END SELECT

    DO i = 1, 3
        rc = WriteE(41, i, ((80_4 * INT(i, 4)) + 10_4))
        IF (rc /= 0_4)  CALL zzrc( rc )
    END DO

    DEALLOCATE( extPtrOut )


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 100_4 )
    END IF



    OPEN(41, ACCESS='stream', ACTION='read',&
            FORM='unformatted', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'OPEN(', iostat, ') ', iomsg
        CALL zzrc( 110_4 )
    END IF


    READ(41, IOSTAT=iostat, IOMSG=iomsg) (baseIn( i ), i = 1, 3)
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 120_4 )
    END IF

    READ(41, IOSTAT=iostat, IOMSG=iomsg) (extendedBaseIn( i ), i = 1, 3)
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 130_4 )
    END IF

    READ(41, IOSTAT=iostat, IOMSG=iomsg) (extendedIn( i ), i = 1, 3)
    IF (iostat /= 0) THEN
        PRINT *, 'READ(', iostat, ') ', iomsg
        CALL zzrc( 140_4 )
    END IF


    CLOSE(41, IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, 'CLOSE(', iostat, ') ', iomsg
        CALL zzrc( 150_4 )
    END IF


    IF ( ANY(baseIn%c /= ca) )          CALL zzrc( 201_4 )
    IF ( ANY(extendedBaseIn%c /= ca) )  CALL zzrc( 202_4 )
    IF ( ANY(extendedIn%c /= ca) )      CALL zzrc( 203_4 )


    DO i = 1, 3
        j = ((i - 1) * 2) + 1
        IF ( ANY(extendedBaseIn( i )%r /= ra( j:(j + 1) )) )&
                                        CALL zzrc( (210_4 + INT(i, 4)) )
        IF ( ANY(extendedIn( i )%r /= ra( j:(j + 1) )) )&
                                        CALL zzrc( (220_4 + INT(i, 4)) )
    END DO

END PROGRAM streamWrite08

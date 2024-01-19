!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October 22, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DIRECT File I/O of Derived Types (with
!*                               Parameters)
!*  SECONDARY FUNCTIONS TESTED : I/O Performed via MODULE Procedures
!*                               (Derived Types passed as POINTER arguments)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE, READ
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To perform an Unformatted READ of an array of Derive Type to a
!*  Sequential file, then WRITE the data.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod
    IMPLICIT NONE

    TYPE b(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1) :: c( l1 )
    END TYPE b

END MODULE bMod


MODULE cMod
    USE bMod

    IMPLICIT NONE

    TYPE c(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: c( l1 )
        TYPE(b(k1,(l1 * 2))) :: b
    END TYPE c

    CONTAINS

        INTEGER FUNCTION Get(cV, u, n)
            TYPE(c(:,4)), POINTER :: cV
            INTEGER :: u
            INTEGER(4) :: n

            INTEGER :: i

            CHARACTER(255) :: iomsg


            READ(u, REC=n, IOSTAT=Get, IOMSG=iomsg) cV
            IF (Get /= 0) THEN
                PRINT *, "READ(", u, ",", n, ",", Get, ") ", iomsg
            END IF

        END FUNCTION Get

        INTEGER FUNCTION Put(cV, u, n)
            TYPE(c(:,4)), POINTER :: cV
            INTEGER :: u
            INTEGER(4) :: n

            INTEGER :: i

            CHARACTER(255) :: iomsg


            WRITE(u, REC=n, IOSTAT=Put, IOMSG=iomsg) cV
            IF (Put /= 0) THEN
                PRINT *, "WRITE(", u, ",", n, ",", Put, ") ", iomsg
            END IF

        END FUNCTION Put

END MODULE cMod


PROGRAM directReadWrite06
    USE cMod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER(4) :: k
    INTEGER(4) :: l
    INTEGER(4) :: m

    INTEGER :: iostat

    CHARACTER(255) :: iomsg

    REAL(4) :: endValues( 150 )
    REAL(4) :: startValues( 150 )

    TYPE(c(5,4)), TARGET :: cArray( 10 )
    TYPE(c(:,4)), POINTER :: pC


    DO i = 1_4, 150_4
        startValues( i ) = 1.0_4 / REAL(i, 4)
    END DO


    OPEN(33, ACCESS='direct', RECL=60,&
         ACTION='readwrite', IOSTAT=iostat, IOMSG=iomsg)
    IF (iostat /= 0) THEN
        PRINT *, "OPEN(", iostat, ") ", iomsg
        CALL zzrc( 10_4 )
    END IF


    DO i = 1_4, 150_4, 15_4
        j = (i / 15_4) + 1_4
        k = i + 14_4
        WRITE(33, REC=j, IOSTAT=iostat, IOMSG=iomsg) startValues( i:k )
        IF (iostat /= 0) THEN
            PRINT *, "WRITE(", j, ",", iostat, ") ", iomsg
            CALL zzrc( (20_4 + i) )
        END IF

        DO l = i, k, 5_4
            m = l + 4_4
            PRINT *, j, "startValues(", l, ":", m, "):"
            PRINT *, "[", startValues( l:m ), "]"
        END DO

        PRINT *
    END DO


    DO i = 10_4, 1_4, -1_4
        pC => cArray( i )
        j = 11_4 - i

        iostat = Get(pC, 33, j)
        IF (iostat /= 0) CALL zzrc( (30_4 + i) )

        PRINT *, j, "cArray(", i, ")%c"
        PRINT *, "[", cArray( i )%c, "]"

        PRINT *, "cArray(", i, ")%b%c( 1:5 )"
        PRINT *, "[", cArray( i )%b%c( 1:5 ), "]"

        PRINT *, "cArray(", i, ")%b%c( 6:10 )"
        PRINT *, "[", cArray( i )%b%c( 6:10 ), "]"

        PRINT *
    END DO


    DO i = 1_4, 10_4
        pC => cArray( i )

        iostat = Put(pC, 33, i)
        IF (iostat /= 0) CALL zzrc( (40_4 + i) )

        PRINT *, i, "cArray(", i, ")%c"
        PRINT *, "[", cArray( i )%c, "]"

        PRINT *, "cArray(", i, ")%b%c( 1:5 )"
        PRINT *, "[", cArray( i )%b%c( 1:5 ), "]"

        PRINT *, "cArray(", i, ")%b%c( 6:10 )"
        PRINT *, "[", cArray( i )%b%c( 6:10 ), "]"

        PRINT *
    END DO


    DO i = 150_4, 1_4, -15_4
        j = 11_4 - (i / 15_4)
        k = i - 14_4
        READ(33, REC=j, IOSTAT=iostat, IOMSG=iomsg) endValues( k:i )
        IF (iostat /= 0) THEN
            PRINT *, "READ(", j, ",", iostat, ") ", iomsg
            CALL zzrc( (50_4 + i) )
        END IF

        DO l = k, i, 5_4
            m = l + 4_4
            PRINT *, j, "endValues(", l, ":", m, "):"
            PRINT *, "[", endValues( l:m ), "]"
        END DO

        PRINT *
    END DO


    IF ( ANY(endValues /= startValues) ) CALL zzrc( 70_4 )

END PROGRAM directReadWrite06

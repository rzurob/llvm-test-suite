!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : idSpecIDValue01 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is a the
!*                               Identifier of a Pending Data Transfer
!*                               Operation on the specified Unit.
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  The value of the expression specified in the ID= specifier shall be the
!*  identifier of a pending data transfer operation for the specified unit.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM idSpecIDValue01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE CheckIDs(ioUnits, aIDs)
            IMPLICIT NONE
            INTEGER, DIMENSION( 10 ), INTENT(IN) :: ioUnits
            INTEGER, DIMENSION( 10,100 ), INTENT(OUT) :: aIDs
        END SUBROUTINE CheckIDs
    END INTERFACE

    INTEGER :: i
    INTEGER(4) :: j

    INTEGER :: iStat = 0

    INTEGER, DIMENSION( 10,100 ) :: aIDs
    INTEGER, DIMENSION( 10,100 ) :: dataValues

    INTEGER, DIMENSION( 10 ), PARAMETER :: ioUnits =&
                (/ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 /)

    CHARACTER(LEN = 256) :: iMsg


    dataValues = RESHAPE((/ ((i, j = 1, 10), i = 1, 100) /), (/ 10,100 /))

    CALL OpenUnits( )


    DO i = 1, 100
        DO j = 1_4, 10_4
            IF (MOD(j, 2) == 1) THEN
                READ(ioUnits( j ), ASYNCHRONOUS='yes', ID=aIDs( j,i ),&
                            &IOSTAT=iStat, IOMSG=iMsg) dataValues( j,i )

                IF (iStat /= 0) THEN
                    WRITE(0, *) "READ(", ioUnits( j ),&
                                ") <", iStat, "> ", iMsg
                    CALL zzrc( (j + 10_4) )
                END IF

            ELSE
                WRITE(ioUnits( j ), ASYNCHRONOUS='yes', ID=aIDs( j,i ),&
                            &IOSTAT=iStat, IOMSG=iMsg) dataValues( j,i )

                IF (iStat /= 0) THEN
                    WRITE(0, *) "WRITE(", ioUnits( j ),&
                                ") <", iStat, "> ", iMsg
                    CALL zzrc( (j + 10_4) )
                END IF
            END IF
        END DO
    END DO


    CALL CheckIDs(ioUnits, aIDs)


    DO i = 10, 1, -1
        CLOSE(ioUnits( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat /= 0) THEN
            WRITE(0, *) "CLOSE(", ioUnits( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (INT(i, 4) + 30_4) )
        END IF
    END DO


    DO i = 1, 100
        WRITE(6, '(5I4)') (dataValues( j,i ), j = 1, 10, 2)
    END DO


    CONTAINS


        SUBROUTINE OpenUnits( )

            INTEGER(4) :: i


            DO i = 1_4, 10_4
                IF (MOD(i, 2) == 1) THEN
                    OPEN(ioUnits( i ), ASYNCHRONOUS='yes', ACTION='read',&
                            &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
                    IF (iStat /= 0) THEN
                        WRITE(0, *) "OPEN(", ioUnits( i ),&
                                    ",Read) <", iStat, "> ", iMsg
                        CALL zzrc( i )
                    END IF

                ELSE
                    OPEN(ioUnits( i ), ASYNCHRONOUS='yes', ACTION='write',&
                             &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
                    IF (iStat /= 0) THEN
                        WRITE(0, *) "OPEN(", ioUnits( i ),&
                                    ",Write) <", iStat, "> ", iMsg
                        CALL zzrc( i )
                    END IF
                END IF
            END DO

        END SUBROUTINE OpenUnits

END PROGRAM idSpecIDValue01


SUBROUTINE CheckIDs(ioUnits, aIDs)
    IMPLICIT NONE

    INTEGER, DIMENSION( 10 ), INTENT(IN) :: ioUnits
    INTEGER, DIMENSION( 10,100 ), INTENT(OUT) :: aIDs

    INTEGER :: i
    INTEGER(4) :: j

    INTEGER :: iStat = 0
    CHARACTER(LEN = 256) :: iMsg


    DO i = 1, 100
        DO j = 1_4, 10_4
            WAIT(ioUnits( j ), ID=aIDs( j,i ), IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat /= 0) THEN
                WRITE(0, *) i, ") WAIT(", ioUnits( j ), ",",&
                            &aIDs( j,i ), ") <", iStat, "> ", iMsg
                CALL zzrc( (j + 20_4) )
            END IF
        END DO
    END DO

END SUBROUTINE CheckIDs

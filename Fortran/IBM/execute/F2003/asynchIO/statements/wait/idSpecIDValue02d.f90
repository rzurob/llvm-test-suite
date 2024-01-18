!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : idSpecIDValue02d - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : ID= Specifier where the Value is a the
!*                               Identifier of a Pending Data Transfer
!*                               Operation on another Unit
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

PROGRAM idSpecIDValue02d

    CHARACTER(LEN = 256) :: iMsg

    INTEGER, DIMENSION( 10 ) :: ids

    INTEGER, DIMENSION( 10 ) :: dataValues = (/ (55, i = 1, 10) /)
    INTEGER, DIMENSION( 10 ) :: units =&
        &(/ 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 /)


    DO i = 1, 10
        IF (MOD(i, 2) == 0) THEN
            OPEN(units( i ), ACTION='read', ASYNCHRONOUS='yes',&
                    &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat <> 0) THEN
                WRITE(0, *) "OPEN(", units( i ), ",Read) <", iStat, "> ", iMsg
                CALL zzrc( i )
            END IF

        ELSE
            OPEN(units( i ), ACTION='write', ASYNCHRONOUS='yes',&
                        &FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat <> 0) THEN
                WRITE(0, *) "OPEN(", units( i ), ",Write) <", iStat, "> ", iMsg
                CALL zzrc( i )
            END IF
        END IF
    END DO


    CALL GetIDs( )


    j = 0
    DO i = 10, 1, -1
        j = j + 1

        WAIT(units( j ), ID=ids( i ), IOSTAT=iStat, IOMSG=iMsg)
        WRITE(6, *) "WAIT(", units( j ), ") <", iStat, "> ", iMsg

        IF (iStat == 0) THEN
            CALL zzrc( (j + 20) )
        END IF
    END DO


    DO i = 1, 10
        CLOSE(units( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "CLOSE(", units( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (i + 30) )
        END IF
    END DO


    WRITE(6, '(10I3)') (dataValues( i ), i = 1, 10)


    CONTAINS

        SUBROUTINE GetIDs( )

            DO i = 1, 10
                IF (MOD(i, 2) == 0) THEN
                    READ(units( i ), ASYNCHRONOUS='yes', ID=ids( i ),&
                            &IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

                    IF (iStat <> 0) THEN
                        WRITE(0, *) "READ(", units( i ),&
                                    &") <", iStat, "> ", iMsg
                        CALL zzrc( (i + 10) )
                    END IF

                ELSE
                    WRITE(units( i ), ASYNCHRONOUS='yes', ID=ids( i ),&
                                &IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

                    IF (iStat <> 0) THEN
                        WRITE(0, *) "WRITE(", units( i ),&
                                    &") <", iStat, "> ", iMsg
                        CALL zzrc( (i + 10) )
                    END IF
                END IF
            END DO

        END SUBROUTINE GetIDs

END PROGRAM idSpecIDValue02d

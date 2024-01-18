!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : noIDSpecPending05 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 14, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() with no ID= Specifier on Pending
!*                               Data Transfer Operations for specific
!*                               Units (mixed Formatted and Unformatted I/O)
!*  SECONDARY FUNCTIONS TESTED : WAIT() on specific ID= Values
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ID= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Since Asynchronous Formatted I/O has been implemented differently from
!*  Asynchronous Unformatted I/O, all of the WAIT() Statments on Units OPEN()ed
!*  for Formatted I/O below should be Successful.
!*
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
!*  If the ID= specifier appears, a wait operation for the specified data
!*  transfer operation is performed. If the ID= specifier is omitted, wait
!*  operations for all pending data transfers for the specified unit are
!*  performed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM noIDSpecPending05

    INTEGER :: formIdx
    INTEGER :: actionIdx

    INTEGER, DIMENSION( 4 ) :: ioUnit = (/ 11, 12, 13, 14 /)

    INTEGER, DIMENSION( 4,10 ) :: ioID
    INTEGER, DIMENSION( 4,10 ) :: dataValues

    CHARACTER(LEN = 11), DIMENSION( 2 ) :: formType
    CHARACTER(LEN = 5), DIMENSION( 2 ) :: actionType = (/ 'write', 'read ' /)

    CHARACTER(LEN = 256) :: iMsg


    formType = (/ 'formatted  ', 'unformatted' /)

    dataValues =&
        &RESHAPE( (/ ((((i * 100) + 25), j = 1, 4), i = 1, 10) /), (/ 4, 10 /))


    DO i = 1, 4
        formIdx = ((i - 1) / 2) + 1
        actionIdx = MOD(i, 2) + 1

        OPEN(ioUnit( i ), ASYNCHRONOUS='yes',&
            &ACCESS='sequential', FORM=formType( formIdx ),&
            &ACTION=actionType( actionIdx ), IOMSG=iMsg, IOSTAT=iStat)

        IF (iStat <> 0) THEN
            WRITE(0, *) "OPEN(", ioUnit( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( i )
        END IF
    END DO


    DO j = 1, 10
        DO i = 1, 4
            actionIdx = MOD(i, 2) + 1

            IF (i == 1) THEN
                READ(ioUnit( i ), '(I4)', ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( i,j ), IOSTAT=iStat) dataValues( i,j )

            ELSE IF (i == 2) THEN
                WRITE(ioUnit( i ), '(I4)', ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( i,j ), IOSTAT=iStat) dataValues( i,j )

            ELSE IF (i == 3) THEN
                READ(ioUnit( i ), ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( i,j ), IOSTAT=iStat) dataValues( i,j )

            ELSE
                WRITE(ioUnit( i ), ASYNCHRONOUS='yes', IOMSG=iMsg,&
                    &ID=ioID( i,j ), IOSTAT=iStat) dataValues( i,j )
            END IF

            IF (iStat <> 0) THEN
                WRITE(0, *) j, ".", i, ")", actionType( actionIdx ),&
                            &"(", ioUnit( i ), ") <", iStat, "> ", iMsg
                CALL zzrc( (10 + j) )
            END IF
        END DO
    END DO


    DO i = 1, 4, 3
        WAIT(ioUnit( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT(", ioUnit( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF
    END DO


    DO j = 1, 10
        DO i = 1, 4
            k = MOD(i, 3)
            formIdx = ((i - 1) / 2) + 1

            WAIT(ioUnit( i ), ID=ioID( i,j ), IOSTAT=iStat, IOMSG=iMsg)

            IF ((formIdx == 1)  .OR.  (k <> 1)) THEN
                IF (iStat <> 0) THEN
                    WRITE(0, *) j, ".", i, ") WAIT(", ioUnit( i ),&
                                            &") <", iStat, "> ", iMsg
                    CALL zzrc( (30 + j) )
                END IF

            ELSE
                IF (iStat == 0) THEN
                    WRITE(0, *) j, ".", i, ") WAIT(", ioUnit( i ),&
                                            &") <", iStat, "> ", iMsg
                    CALL zzrc( (30 + j) )
                END IF
            END IF
        END DO
    END DO


    DO i = 4, 1, -1
        CLOSE(ioUnit( i ), IOSTAT=iStat, IOMSG=iMsg)

        IF (iStat <> 0) THEN
            WRITE(0, *) "CLOSE(", ioUnit( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (40 + i) )
        END IF
    END DO


    DO i = 1, 10
        WRITE(6, '(I4)') dataValues( 1,i )

        DO j = 2, 4
            IF (dataValues( 1,i ) <> dataValues( j,i )) THEN
                WRITE(0, *) "dataValues( 1,", i, ") = '",&
                                    &dataValues( 1,i ), "'"
                WRITE(0, *) "dataValues( ", j, ",", i, ") = '",&
                                    &dataValues( j,i ), "'"
                CALL zzrc( (50 + i) )
            END IF
        END DO
    END DO

END PROGRAM noIDSpecPending05

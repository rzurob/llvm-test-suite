!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C1232arraySect01 - ASYNCHRONOUS
!*                               Attribute in Array Section Arguments
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April  6, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Actual Argument is an Array Section
!*  SECONDARY FUNCTIONS TESTED : Dummy Argument explicitly has the ASYNCHRONOUS
!*                               Attribute and is an Assumed-Shape Array
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASYNCHRONOUS Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  12.4.1.2 Actual arguments associated with dummy data objects
!*
!*  If the actual argument is an array section having a vector subscript,
!*  the dummy argument is not definable and shall not have the INTENT (OUT),
!*  INTENT (INOUT), VOLATILE, or ASYNCHRONOUS attributes.
!*
!*  C1232 (R1221) If an actual argument is an array section or an
!*                assumed-shape array, and the corresponding dummy argument
!*                has either the VOLATILE or ASYNCHRONOUS attribute, that
!*                dummy argument shall be an assumed-shape array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program C1232arraySect01

    INTERFACE
        subroutine ReportData(n, theData)
            INTEGER, INTENT(IN) :: n
            COMPLEX, ASYNCHRONOUS, DIMENSION( : ) :: theData
        end subroutine ReportData
    END INTERFACE
    
    INTEGER, DIMENSION( 1000 ) :: ioID

    COMPLEX, DIMENSION( 1000 ) :: someData

    CHARACTER(LEN = 256) :: iMsg


    OPEN(1232, FILE='C1232arraySect01.dat', IOMSG=iMsg,&
            ASYNCHRONOUS='yes', ACCESS='direct', RECL=8,&
            ACTION='write', FORM='unformatted', IOSTAT=iStat)
    IF (0 <> iStat) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 0, 9
        k = i * 100

        DO j = 1, 100
            l = k + j

            someData( l ) = CMPLX( i,j )

            WRITE(1232, ASYNCHRONOUS='yes', ID=ioID( l ),&
                REC=l, IOSTAT=iStat, IOMSG=iMsg) someData( l )

            IF (0 <> iStat) THEN
                WRITE(0, *) i, ",", j, ") WRITE() <", iStat, "> ", iMsg
                CALL zzrc( 2 )
            END IF
        END DO

        CALL ReportData(100, someData( (k + 1):(k + 100) ))

        IF (i > 0) THEN
            j = k - 100
            DO l = (j + 1), (j + 100)
                WAIT(1232, ID=ioID( l ), IOSTAT=iStat, IOMSG=iMsg)

                IF (0 <> iStat) THEN
                    WRITE(0, *) l, ") WAIT(ID=", ioID( l ),&
                                    ") <", iStat, "> ", iMsg
                    CALL zzrc( 3 )
                END IF
            END DO
        END IF
    END DO


    DO l = 901, 1000
        WAIT(1232, ID=ioID( l ), IOSTAT=iStat, IOMSG=iMsg)
        IF (0 <> iStat) THEN
            WRITE(0, *) l, ") WAIT(ID=", ioID( l ), ") <", iStat, "> ", iMsg
            CALL zzrc( 4 )
        END IF
    END DO


    CLOSE(1232, IOSTAT=iStat, IOMSG=iMsg)
    IF (0 <> iStat) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF

end program C1232arraySect01


subroutine ReportData(n, theData)

    INTEGER, INTENT(IN) :: n
    COMPLEX, ASYNCHRONOUS, DIMENSION( : ) :: theData


    m = n / 5
    DO i = 0, (m - 1)
        WRITE(6, '(5(" (",F6.2,",",F6.2,")"))')&
            (theData( j ), j = ((i * 5) + 1), ((i * 5) + 5))
    END DO

end subroutine ReportData

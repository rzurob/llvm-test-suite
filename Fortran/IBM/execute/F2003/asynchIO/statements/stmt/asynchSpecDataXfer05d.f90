!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : January 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in Data Transfer
!*                               Statements
!*  SECONDARY FUNCTIONS TESTED : WRITE(UNIT=OUTPUT_UNIT) -- STDOUT (UNIT=6)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*  To perform an Asynchronous Write to the Pre-Connected I/O Unit STDOUT
!*  using the Named Constant as well as the actual Unit Number.
!*
!*  9.4 File connection
!*
!*  R901 io-unit  is  file-unit-number
!*                or  *
!*  ...
!*
!*  An asterisk identifies particular processor-dependent external units
!*  that are preconnected for formatted sequential access (9.5.3.2). These
!*  units are also identified by unit numbers defined by the named constants
!*  INPUT_UNIT and OUTPUT UNIT of the ISO FORTRAN ENV module (13.8.2).
!*
!*  9.4.1 Connection modes
!*
!*  Values for the modes of a connection are established when the connection
!*  is initiated. ...  If the connection is initiated other than by an OPEN
!*  statement (that is, if the file is an internal file or preconnected file)
!*  the values established are those that would be implied by an initial OPEN
!*  statement without the corresponding keywords.
!*
!*  9.4.5.3 ASYNCHRONOUS= specifier in the OPEN statement
!*
!*  ... If this specifier is omitted, the default value is NO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchSpecDataXfer05d
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: ioStatus

    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  Asynchronous Write to Unit 6 (STDOUT).
    !
    WRITE(6, *, ASYNCHRONOUS='Yes', IOSTAT=ioStatus, IOMSG=ioErrorMsg) i

    PRINT *, "WRITE(6,ASYNCHRONOUS='yes'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg

    IF (ioStatus <> 169) THEN
        CALL zzrc( 1 )
    END IF


    !
    !  Asynchronous Write to OUTPUT_UNIT (STDOUT).
    !
    WRITE(UNIT=OUTPUT_UNIT, ASYNCHRONOUS='yeS',&
                &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i

    PRINT *, "WRITE(OUTPUT_UNIT,ASYNCHRONOUS='yes'): ioStatus = (",&
                                            &ioStatus, ") ", ioErrorMsg

    IF (ioStatus <> 169) THEN
        CALL zzrc( 2 )
    END IF


END PROGRAM asynchSpecDataXfer05d

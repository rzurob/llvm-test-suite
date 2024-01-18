!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : January 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in Data Transfer
!*                               Statements
!*  SECONDARY FUNCTIONS TESTED : WRITE(UNIT=ERROR_UNIT) -- STDERR (UNIT=0)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WRITE(), ASYNCHRONOUS= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*  To perform an Asynchronous Write to the Pre-Connected I/O Unit STDERR
!*  using the Named Constant as well as the actual Unit Number.
!*
!*  9.4 File connection
!*
!*  R901 io-unit  is  file-unit-number
!*                or  *
!*  ...
!*
!*  This standard identifies a processor-dependent external unit for the
!*  purpose of error reporting. This unit shall be preconnected for sequential
!*  formatted output. The processor may define this to be the same as the
!*  output unit identified by an asterisk. This unit is also identified by
!*  a unit number defined by the named constant ERROR_UNIT of the
!*  ISO FORTRAN ENV intrinsic module.
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

PROGRAM asynchSpecDataXfer06d
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: ioStatus

    CHARACTER(len = 256) :: ioErrorMsg


    !
    !  Asynchronous Write to ERROR_UNIT (STDERR).
    !
    WRITE(ERROR_UNIT, *, ASYNCHRONOUS='yes',&
            &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i

    PRINT *, "WRITE(ERROR_UNIT,ASYNCHRONOUS='yes'): ioStatus = (",&
                                            &ioStatus, ") ", ioErrorMsg

    IF (ioStatus <> 169) THEN
        CALL zzrc( 1 )
    END IF


    !
    !  Asynchronous Write to Unit 0 (STDERR).
    !
    WRITE(UNIT=0,&
            &ASYNCHRONOUS='yes                                  ',&
                                    &IOSTAT=ioStatus, IOMSG=ioErrorMsg) i

    PRINT *, "WRITE(0,ASYNCHRONOUS='yes'): ioStatus = (",&
                                    &ioStatus, ") ", ioErrorMsg

    IF (ioStatus <> 169) THEN
        CALL zzrc( 2 )
    END IF


END PROGRAM asynchSpecDataXfer06d

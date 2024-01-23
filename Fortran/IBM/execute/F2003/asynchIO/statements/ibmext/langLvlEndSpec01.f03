!*  ===================================================================
!*
!*                               Language Level Options
!*
!*  DATE                       : April 11, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : -qlanglvl=[extended|2003pure|2003std] Option
!*  SECONDARY FUNCTIONS TESTED : END= Specifier in the READ() Statements (I/O
!*                               on a Direct File)
!*
!*                               langLvlEndSpec01p:  xlf95
!*                               langLvlEndSpec01s:  xlf95
!*  REQUIRED COMPILER OPTIONS  : langLvlEndSpec01e:  -qlanglvl=extended
!*                               langLvlEndSpec01p:  -qlanglvl=2003pure
!*                               langLvlEndSpec01s:  -qlanglvl=2003std
!*
!*  KEYWORD(S)                 : READ(), WAIT(), END= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*  The END= Specifier should not be useable when a Direct File is being
!*  accessed.  Code which uses the IBM Extension(s) noted below.
!*
!*  From the F2003 Standard:
!*
!*  9.10 Error, end-of-record, and end-of-file conditions
!*
!*  An end-of-file condition occurs in the following cases:
!*
!*  (1) When an endfile record is encountered during the reading of a file
!*      connected for sequential access.
!*  (2) When an attempt is made to read a record beyond the end of an
!*      internal file.
!*  (3) When an attempt is made to read beyond the end of a stream file.
!*
!*  9.10.2 End-of-file condition and the END= specifier
!*
!*  If an end-of-file condition occurs during execution of an input/output
!*  statement that contains neither an END= specifier nor an IOSTAT=
!*  specifier, execution of the program is terminated. If an end-of-file
!*  condition occurs during execution of an input/output statement that
!*  contains either an END= specifier or an IOSTAT= specifier, and an error
!*  condition does not occur then ...
!*
!*  READ
!*
!*    Purpose
!*      The READ statement is the data transfer input statement.
!*
!*      Syntax
!*        READ ( io_control_list ) input_item_list
!*
!*         ...
!*
!*         END= stmt_label
!*           is an end-of-file specifier that specifies a statement label
!*           at which the program is to continue if an endfile record is
!*           encountered and no error occurs. ...  This specifier can be
!*           specified for a unit connected for either sequential or
!*           direct access.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM langLvlEndSpec01
    USE ISO_FORTRAN_ENV

    INTEGER, DIMENSION( 3 ) :: ioID
    INTEGER, DIMENSION( 4 ) :: dataValue

    INTEGER, DIMENSION( 4 ) :: recNum = (/ 1, 22, 3, 4/)

    CHARACTER(LEN = 256) :: iMsg


    OPEN(21, action='read', access='direct',&
        recl=4, asynchronous='yes', form='unformatted')


    DO i = 1, 3
        READ(21, asynchronous='yes',&
            rec=recNum( i ), id=ioID( i )) dataValue( i )
    END DO


    !
    !  IBM Specific Code:  END= Specifier on a Unit OPEN()ed for Direct
    !  Input.
    !
    READ(21, rec=4, END=100, iostat=iStat, iomsg=iMsg) dataValue( 4 )
    WRITE(6, *) "READ() <", iStat, "> ", iMsg
    ERROR STOP 11

    !
    !  IOSTAT= Specifier is set to IOSTAT_END (XL Fortran Language Reference
    !  -- Chapter:  "XL Fortran Input/Output", Section:  "Conditions and
    !  IOSTAT values", Sub-Section:  "End-of-file conditions"
    !
100 WRITE(6, *) "READ(END=100) <", iStat, "> ", iMsg
    IF (IOSTAT_END /= iStat) THEN
        ERROR STOP 12
    END IF


    DO i = 3, 1, -1
        WAIT(21, id=ioID( i ), iostat=iStat, iomsg=iMsg)
        IF (224 /= iStat) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( i + 20 )
        END IF
    END DO


    CLOSE( 21 )

END PROGRAM langLvlEndSpec01

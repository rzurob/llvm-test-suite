! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specifier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Testing NEWUNIT with List-directed
!*                                I/O, Delim=apostrophe,read,write, formattedI/O
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT13
     IMPLICIT NONE

     INTEGER       ios,IVAR
     INTEGER*2  i2_in, i2_out
     INTEGER*4  i4_in, i4_out
     INTEGER*8  i8_in, i8_out

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

     i2_in = 1234
     i4_in = -20000000
     i8_in = 1234567890

!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

     OPEN(NEWUNIT=IVAR, FILE='fxstio157.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, DELIM='APOSTROPHE')

     WRITE(IVAR, FMT=*, IOSTAT=ios, ERR=91, POS=10) &
    &      1234, -20000000, (i8_in - 8*4 + 32)

     READ(IVAR, FMT=*, IOSTAT=ios, ERR=92, POS=10) i2_out, i4_out, i8_out

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     IF ( i2_in .ne. i2_out ) THEN
     PRINT *, i2_in,i2_out
     ERROR STOP 11_4
     END IF

     IF ( i4_in .ne. i4_out ) THEN
     PRINT *, i4_in,i4_out
     ERROR STOP 12_4
     END IF

     IF ( i8_in .ne. i8_out ) THEN
     PRINT *, i8_in,i8_out
     ERROR STOP 13_4
     END IF

     CLOSE(IVAR, STATUS='DELETE')

     RETURN

!**********************************************************
!        Checking for Error                               *
!**********************************************************

90   PRINT *, "Error while openning the file: IOSTAT = ", ios
     ERROR STOP 90_4

91   PRINT *, "Error while writing to the file: IOSTAT = ", ios
     ERROR STOP 91_4

92   PRINT *, "Error while reading from the file: IOSTAT = ", ios
     ERROR STOP 92_4

93   PRINT *, "Error while rewinding the file: IOSTAT = ", ios
     ERROR STOP 93_4

    END PROGRAM FXOPEN_NEWUNIT13

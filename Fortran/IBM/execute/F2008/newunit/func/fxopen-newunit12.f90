! *********************************************************************
!* ===================================================================
!*
!* DATE                         : Oct. 2010
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: NEWUNIT= specIFier, Feature#:377344
!* SECONDARY FUNTIONS TESTED    : READ,WRITE,REWIND
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Testing NEWUNIT with List-directed
!*                                I/O, Delim=quote,read,write, formattedI/O
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  10/10/21    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    PROGRAM FXOPEN_NEWUNIT12
     IMPLICIT NONE

     INTEGER    ios,IVAR
     INTEGER*1  i1_in, i1_out
     INTEGER*2  i2_in, i2_out
     INTEGER*4  i4_in, i4_out
     INTEGER*8  i8_in, i8_out

!**********************************************************
!        Initialization of variables                      *
!**********************************************************

     i1_in = 12
     i2_in = 1234
     i4_in = -20000000
     i8_in = 1234567890

!**********************************************************
!        Writing and Reading the file                     *
!**********************************************************

     OPEN(NEWUNIT=IVAR, FILE='fxstio151.dat', FORM='FORMATTED', ACCESS='STREAM', &
    &     STATUS='REPLACE', IOSTAT=ios, ERR=90, DELIM="QUOTE")

     WRITE(IVAR, FMT=*, IOSTAT=ios, ERR=91, POS=10) i1_in, i2_in, i4_in, i8_in

     REWIND(IVAR, IOSTAT=ios, ERR=93)

     READ(IVAR, FMT=*, IOSTAT=ios, ERR=92, POS=10) i1_out, i2_out, i4_out, i8_out

!**********************************************************
!        Checking the Results                             *
!**********************************************************

     IF ( i1_in .ne. i1_out ) THEN
     PRINT *, i1_in,i1_out
     ERROR STOP 10_4
     END IF

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
     ERROR STOP 90
91   PRINT *, "Error while writing to the file: IOSTAT = ", ios
     ERROR STOP 91
92   PRINT *, "Error while reading from the file: IOSTAT = ", ios
     ERROR STOP 92
93   PRINT *, "Error while rewinding the file: IOSTAT = ", ios
     ERROR STOP 93

    END PROGRAM FXOPEN_NEWUNIT12

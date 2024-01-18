!*  ===================================================================
!*
!*  DATE                       : March  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : UNIT= Specifier Value is *NOT* a
!*                               scalar-int-expr
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), UNIT= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
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
!*  C939 (R922) A file-unit-number shall be specified; if the optional
!*              characters UNIT= are omitted, the file-unit-number shall
!*              be the first item in the wait-spec-list.
!*
!*  9.4 File connection
!*
!*  A unit, specified by an io-unit, provides a means for referring to a file.
!*
!*  R901 io-unit           is  file-unit-number
!*                         ...
!*
!*  R902 file-unit-number  is  scalar-int-expr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM unitSpecFileUnitNum02d

    CHARACTER(LEN = 5) :: internalFile = ' 2600'

    CHARACTER(LEN = 256) :: iMsg


    WRITE(*, '(A5)') internalFile
    WAIT(*, IOSTAT=i, IOMSG=iMsg)

    WRITE(FMT='(A5)', UNIT=*) internalFile
    WAIT(IOSTAT=i, UNIT=*, IOMSG=iMsg)


    READ(FMT='(I5)', UNIT=internalFile) i
    WAIT(internalFile, IOMSG=iMsg, IOSTAT=i)

    READ(internalFile, '(I5)') i
    WAIT(IOMSG=iMsg, IOSTAT=i, UNIT=internalFile)

END PROGRAM unitSpecFileUnitNum02d

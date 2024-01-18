!*  ===================================================================
!*
!*  DATE                       : March  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : Multiple UNIT= Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), UNIT= Specifier
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
!*
!*  C938 (R922) No specifier shall appear more than once in a given
!*              wait-spec-list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C938UnitSpec02d

    CHARACTER(LEN = 256) :: iMsg


    OPEN(UNIT=8200, ASYNCHRONOUS='yes',&
        &ACCESS='sequential', FORM='formatted')


    READ(8200, '(I4)', ASYNCHRONOUS='yes', ID=ioID) iVar

    WAIT(UNIT=8200, ID=ioID, IOSTAT=iStat, UNIT=9, IOMSG=iMsg)


    CLOSE( UNIT=8200 )

END PROGRAM C938UnitSpec02d

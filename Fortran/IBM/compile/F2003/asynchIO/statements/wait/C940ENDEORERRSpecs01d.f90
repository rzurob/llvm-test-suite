!*  ===================================================================
!*
!*  DATE                       : March 15, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : Invalid Statement Label(s) for the END=,
!*                               EOR=, and ERR= Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), END= Specifier, EOR= Specifier,
!*                               ERR= Specifier
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
!*  C940 (R922) The label in the ERR=, EOR=, or END= specifier shall be
!*              the statement label of a branch target statement that
!*              appears in the same scoping unit as the WAIT statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C940ENDEORERRSpecs01d

    OPEN(940, ASYNCHRONOUS='yes')

    WRITE(940, ASYNCHRONOUS='yes') 940

100 CALL WaitOnIO( 940 )

    CLOSE( 940 )

200 CONTAINS

        SUBROUTINE WaitOnIO( ioUnit )

            INTEGER :: ioUnit

            WAIT(ioUnit, END=100, EOR=200, ERR=300)

            RETURN

!300        WRITE(0, *) "WAIT(", ioUnit, ")"

        END SUBROUTINE WaitOnIO

END PROGRAM C940ENDEORERRSpecs01d

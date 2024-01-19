!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN=specifier in INQUIRE
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in INQUIRE statement
!*                               1 If no sign= specifier is specified, the
!*                                 PROCESSOR_DEFINED is the default
!*                                 SIGN mode
!*                               2 If the connection isn't for formatted
!*                                 output or there is no connection,
!*                                 it is assigned value: UNDEFINED
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
character*17 sign_mode

open(5, file='sign03.tmp')
inquire (5, sign=sign_mode)
close(5)
if (sign_mode .ne. "PROCESSOR_DEFINED") error stop 1

open(5, file='sign03.tmp', sign='plus')
inquire (5, sign=sign_mode)
close(5)
if (sign_mode .ne. "PLUS") error stop 2

open(5, file='sign03.tmp', sign='suppress')
inquire (5, sign=sign_mode)
close(5)
if (sign_mode .ne. "SUPPRESS") error stop 3

open(5, file='sign03.tmp', form="unformatted", sign='suppress')
inquire (5, sign=sign_mode)
close(5)
if (sign_mode .ne. "UNDEFINED") error stop 4

inquire (9, sign=sign_mode)
if (sign_mode .ne. "UNDEFINED") error stop 5
end

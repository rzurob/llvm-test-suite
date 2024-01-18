!***********************************************************************
!*
!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN= specifier
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the error message of the
!*                               SIGN= specifier caught in FE
!*                               1 The invalid sign= specifier in OPEN statement
!*                                 and  WRITE statement
!*                               2 The sign= not allowed in READ
!*                               3 The sign= used in unformatted output
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
integer m

!* The error messages will be caught in FE
!* Invalid sign= value for OPEN  statements
open (1, file = "sign08.out", sign="aaa")

!* Invalid sign= value for WRITE  statements
write (1, *, sign="aaa")
close (1)

!* sign= used for unformatted connection
open (1, file = "sign09.out", form="unformatted", sign="suppress")
close (1)

!* The sign= isn't allowed for READ statement
read (*, *, sign="plus") m

end

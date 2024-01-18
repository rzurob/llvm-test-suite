!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -rf sign06.out
! %COMPOPTS:-qfree=f90
! %GROUP: sign06.f
! %VERIFY: sign06.vf:sign06.out
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Test of SIGN= specifier
!*      
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN=specifier in namelist output
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in namelist output
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
real r
integer m
namelist /nl/ r, m     
r = 10.5
m = 20

open (1, file="sign06.out")
!* The sign mode of nameslist output is set to plus *!
write(1, nl, sign="plus")

!* The sign mode of nameslist output is set to suppress *!
write(1, nl, sign="suppress")

!* The sign mode of nameslist output is set to processor_defined *!
!*  which should act the same as suppress *!
write(1, nl, sign="processor_defined")
close(1)

!* The sign mode in OPEN is set to plus which will be overwrite by
!* WRITE statement for namelist output
open (1, file="sign06.out", sign="plus", status="old", position="append")
write(1, nl, sign="suppress")
close(1)

!* The sign mode in OPEN is set to suppress which will be overwrite by
!* WRITE statement for namelist output
open (1, file="sign06.out", sign="suppress", status="old", position="append")
write(1, nl, sign="plus")
close(1)

end

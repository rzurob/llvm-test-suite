!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : December 12, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic test case for ENCODING=specifier
!*                               in OPEN statement
!*                               with specifers set to UTF-8
!*
!234567890093456789009345678900934567890093456789009345678900934567890

program main
  INTEGER ::x
  character(20):: encoding_mode2 ='UTF-8'

  open (UNIT=1, FILE='real4.dat', ENCODING="UTF-8")
  close(1)

end


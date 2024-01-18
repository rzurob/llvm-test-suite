!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : encodingspecifier010
!*
!*  PROGRAMMER                 : Morteza Ershad-Manesh
!*  DATE                       : December 12, 2007
!*  ORIGIN                     : XL Fortran test Team,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Diagnostic test case for ENCODING=specifier
!*                               in OPEN statement
!*                               with specifers set to UTF-8
!*                              
!*
!234567890093456789009345678900934567890093456789009345678900934567890

program main
  INTEGER ::x
  character(20):: encoding_mode2 ='UTF-8'

  open (UNIT=1, FILE='real4.dat', ENCODING="UTF-8")
  close(1)

end


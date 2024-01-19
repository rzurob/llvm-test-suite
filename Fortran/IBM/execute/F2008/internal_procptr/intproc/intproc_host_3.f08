!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 27 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test host instance --
!*    Call sequence -- access various host environment
!*    (388472, 388871)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  INTEGER :: count=1

  CONTAINS

  SUBROUTINE mod_sub(proc)
  PROCEDURE() :: proc
  INTEGER :: iii
  PROCEDURE(), POINTER :: procptr
  iii = 3
  procptr => mod_int

  CALL proc(mod_int)

  CONTAINS
    RECURSIVE SUBROUTINE mod_int(proc)
    PROCEDURE() :: proc
    IF ( iii /= 3) ERROR STOP 13
    count = count + 1

    !Limit the size of the call chain.
    print*, count
    IF ( count == 100 ) ERROR STOP 0

    !CALL proc(mod_int)
    CALL proc(procptr)

    END SUBROUTINE
  END SUBROUTINE
  END MODULE


  PROGRAM intproc_host_3
  USE M
  EXTERNAL Extsub
  INTEGER :: iii

  iii = 1
  CALL Extsub(main_int)

  CONTAINS
    SUBROUTINE Main_int(proc)
    PROCEDURE() :: proc
    IF ( iii /= 1) ERROR STOP 11

    CALL proc(mod_sub)
    END SUBROUTINE
  END

  SUBROUTINE extsub(proc)
  PROCEDURE() :: proc
  INTEGER :: iii
  PROCEDURE(), POINTER :: procptr

  iii = 2
  procptr => ext_int
  CALL proc(ext_int)

  CONTAINS
    RECURSIVE SUBROUTINE ext_int(proc)
    PROCEDURE() :: proc
    IF ( iii /= 2) ERROR STOP 12

    !CALL proc(ext_int)
    CALL proc(procptr)

    END SUBROUTINE
  END SUBROUTINE


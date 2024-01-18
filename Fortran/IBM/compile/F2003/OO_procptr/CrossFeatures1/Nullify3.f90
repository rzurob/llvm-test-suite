! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp  Nullify3.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Nullify3.f
!*
!*  DATE                       : Aug. 17, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The nullify stmt
!*  Refer 308490 - Nullify a function return!
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Nullify3

    interface
        function aaa ()
            integer, allocatable :: aaa
        end function
    end interface

    procedure(aaa) a1

    nullify (a1())

  END




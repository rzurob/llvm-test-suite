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
! %POSTCMD:  tcomp Misc7.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc7.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 30, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*   
!*  Procedure pointer - bindc label
!*  (306276/315101)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc7 
  IMPLICIT NONE 

  INTERFACE
    SUBROUTINE IS() BIND(C)
    END SUBROUTINE

    SUBROUTINE IS1() BIND(C, NAME="is1")
    END SUBROUTINE

    SUBROUTINE NoBindC()
    END SUBROUTINE

    SUBROUTINE BindC() BIND(C)
    END SUBROUTINE

    SUBROUTINE BindC1() BIND(C, NAME="BindC")
    END SUBROUTINE

  END INTERFACE

  PROCEDURE(IS),  BIND(C, NAME="a")   :: A

  PROCEDURE(IS),  BIND(C, NAME="is")  :: B

  PROCEDURE(IS1), BIND(C, NAME="a")   :: C

  PROCEDURE(IS1), BIND(C          )   :: D

  PROCEDURE(IS1), BIND(C, NAME="is")  :: E

  PROCEDURE(IS1), BIND(C, NAME="is1") :: F

  PROCEDURE(IS), BIND(C            )  :: IS1 

  PROCEDURE(IS), BIND(C, NAME="Ptr"), POINTER  :: ProcPtr 

  PROCEDURE(IS),  BIND(C, NAME="G")   :: G

  PROCEDURE(BindC),    POINTER :: ProcPtr1
  PROCEDURE(ProcPtr1), POINTER :: ProcPtr2
  PROCEDURE(BindC1),   POINTER :: ProcPtr3
  PROCEDURE(NoBindC),  POINTER :: ProcPtr4
  PROCEDURE(),         POINTER :: ProcPtr5

  ProcPtr1 => NoBindC
  ProcPtr2 => NoBindC
  ProcPtr3 => NoBindC
  ProcPtr4 => BindC
  ProcPtr5 => BindC  ! this is fine
  END


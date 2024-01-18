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
! %POSTCMD: tcomp Associated5.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Associated5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun 20, 2005
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
!*  Procedure pointer with data target. 
!*   
!*  (ICE-314866/317312) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
    END TYPE
  
    CONTAINS

    FUNCTION ModFun()
    CLASS(DT), POINTER :: ModFun
      ALLOCATE(ModFun, SOURCE=DT())
    END FUNCTION

    SUBROUTINE ModSub()
    END SUBROUTINE

  END MODULE

  PROGRAM Associated5 
  USE M
  IMPLICIT NONE 

  CONTAINS

  SUBROUTINE IntSub(ClassTar)
 
  PROCEDURE(ModFun), POINTER :: ProcPtr 
  CLASS ( DT ),      TARGET  :: ClassTar 
  CLASS(DT),         POINTER :: DataPtr

  PROCEDURE(), POINTER       :: ProcPtr1
  TYPE(DT),    TARGET        :: DTTar

  ProcPtr => ClassTar 

  DataPtr => ModFun 

  DataPtr => ProcPtr 

  DataPtr => ProcPtr1

  DataPtr => ModSub

  ProcPtr1 => DTTar

  END SUBROUTINE
 
  END

 

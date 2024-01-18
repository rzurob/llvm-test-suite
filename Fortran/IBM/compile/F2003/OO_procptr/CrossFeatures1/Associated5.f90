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
!*  DATE                       : May. 9, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id = 0
    END TYPE
  END MODULE

  PROGRAM Associated5 
  USE M
  IMPLICIT NONE 
  
  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr=>NULL() 
  TYPE ( DT ),   TARGET        :: V

  TYPE(DT), POINTER    :: DataPtr=>NULL()
  PROCEDURE (TYPE(DT)) :: Fun 

  PROCEDURE(TYPE(DT)), POINTER :: ProcPtr1=>NULL() 
  TYPE(DT),            POINTER :: DataPtr1=>NULL()



  ProcPtr => V 

  DataPtr => Fun

  ProcPtr1 => DataPtr1

  DataPtr1 => ProcPtr1
   
  END

  FUNCTION Fun()
  USE M
  TYPE (DT) :: Fun
    Fun = DT(-1)
  END FUNCTION
 

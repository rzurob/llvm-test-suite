! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp3.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp3.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 18, 2005
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
!*  Structure component - initialization-expr 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
!     SEQUENCE  !?
      INTEGER :: BaseID=1
      PROCEDURE(LOGICAL(2)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT
!     SEQUENCE
      INTEGER :: ChildID=2
      TYPE(Base) :: BComp=Base(-1, NULL())
    END TYPE

  END MODULE


  PROGRAM StrComp3  
  USE M
  IMPLICIT NONE 

  TYPE(DT) :: U
  TYPE(DT) :: V=DT(-2,     &
                & Base(-1, NULL()))


  IF ( V%ChildId .NE. -2 ) STOP 12
  IF ( V%BComp%BaseId .NE. -1 ) STOP 11
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) STOP 13

  IF ( U%ChildId .NE. 2 ) STOP 22
  IF ( U%BComp%BaseId .NE. -1 ) STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr) ) STOP 23


  END


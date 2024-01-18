! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/OO_procptr/CrossFeatures1/StrComp4.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp4.f 
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
!*  TEST CASE NAME             : StrComp4.f 
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
!*  Structure component - Paramter 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseID=1
      PROCEDURE(LOGICAL(2)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT(K2)    ! (4)
      INTEGER, KIND  :: K2
      INTEGER(K2)    :: ChildID=2
      TYPE(Base(K2)) :: BComp=Base(K2)(-1, NULL())
    END TYPE
 
  END MODULE

  PROGRAM StrComp4  
  USE M
  IMPLICIT NONE 

  TYPE(DT(4)), PARAMETER :: Para=DT(4)(-2,              &
                           &     Base(4)(-3, NULL()))

  TYPE(DT(4)), PARAMETER :: U=Para
  TYPE(DT(4))            :: V

  IF ( Para%ChildId .NE. -2 ) STOP 12
  IF ( Para%BComp%BaseId .NE. -3 ) STOP 11
  IF ( ASSOCIATED(Para%BComp%ProcPtr) ) STOP 13

  IF ( U%ChildId .NE. -2 ) STOP 22
  IF ( U%BComp%BaseId .NE. -3 ) STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr) ) STOP 23

  V = Para
  IF ( V%ChildId .NE. -2 ) STOP 22
  IF ( V%BComp%BaseId .NE. -3 ) STOP 21
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) STOP 23


  END


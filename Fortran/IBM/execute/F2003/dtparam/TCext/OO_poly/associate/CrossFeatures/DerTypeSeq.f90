! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/DerTypeSeq.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  DerTypeSeq.f  
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
!*  TEST CASE NAME             : DerTypeSeq
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 07, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is of a derived sequence type  
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      SEQUENCE
      INTEGER(K1)   :: BaseId = 1
    END TYPE

  END MODULE 

  MODULE M1
  USE M0, DT0=>Base

    TYPE :: Child(K2)    ! (4)
      INTEGER, KIND :: K2
      SEQUENCE
      INTEGER(K2)   :: ChildId = 2
    END TYPE

  END MODULE

  MODULE M
  USE M1, DT=>Child

  TYPE(DT(4)), SAVE :: T
  TYPE(DT0(4)), SAVE :: T0

  END MODULE
  
  PROGRAM DerTypeSeq
  USE M, V=>T, V0=>T0
  IMPLICIT NONE


  ASSOCIATE( As => V, As0 => V0 )

    IF (As0%BaseID .NE. 1) STOP 20
    IF (As%ChildID .NE. 2) STOP 21

    As0 = DT0(4)(-1)
    As  = DT(4)(-2)

    IF (V0%BaseID .NE. -1) STOP 30
    IF (V%ChildID .NE. -2) STOP 31

  END ASSOCIATE


  END



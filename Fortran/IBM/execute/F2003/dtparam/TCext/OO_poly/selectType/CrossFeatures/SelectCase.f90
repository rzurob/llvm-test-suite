! GB DTP extension using:
! ftcx_dtp /tstdev/OO_poly/selectType/CrossFeatures/SelectCase.f
! opt variations: -qck

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SelectCase.f 
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
!*  TEST CASE NAME             : SelectCase 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 04, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
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
!* Select Case 
!* (ICE-299302)
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectCase 
  IMPLICIT CLASS(*)(U)
  TYPE :: DT(K1,N1)    ! (4,3) 
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: Int
    CHARACTER(N1) :: C 
  END TYPE
  INTEGER :: i
 
  CALL Sub(DT(4,3)(Int=6, C=""), 6)

  CONTAINS

  SUBROUTINE Sub(U, I)

  SELECT TYPE (U)
  CLASS IS (DT(4,*))
  
    SELECT CASE (U%Int)
    CASE (:5)
      STOP 20
    CASE (6)
      PRINT *, "1-OK!"
    CASE (7:)
      STOP 20
    END SELECT

  
    SELECT CASE (I)
    CASE (:5)
      STOP 20
    CASE (6)
      PRINT *, "2-OK!"
    CASE (7:)
      STOP 20
    END SELECT


  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END




! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/C811ArrVec.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C811ArrVec.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrVec
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811 
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
!*    The selector is an array section  without ssociate-name => 
!*    Array section with a vector subscript. 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    END TYPE

  END MODULE 

  PROGRAM C811ArrVec
  USE M
  IMPLICIT NONE
  
  CLASS(Base(4,:)), POINTER :: Ptr(:,:)

  ALLOCATE( Child(4,20) :: Ptr(2:10, 3:12) )
 
  SELECT TYPE ( Ptr((/10,7,7,2/), (/12,3,3,12/)) )
    TYPE IS (Base(4,*))
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT 
  STOP 40

  END


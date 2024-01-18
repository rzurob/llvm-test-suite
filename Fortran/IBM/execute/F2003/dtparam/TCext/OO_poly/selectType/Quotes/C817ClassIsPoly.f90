! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/Quotes/C817ClassIsPoly.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: C817ClassIsPoly.f 
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
!*  TEST CASE NAME             : C817ClassIsPoly
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 3, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C817 
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
!*    The type guard CLASS IS is specified with the same type twice
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0(K1,N1)    ! (4,20) 
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1(N2,K2)    ! (4,20,20,4) 
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2(N3,K3)    ! (4,20,20,4,20,4) 
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3(N4,K4)    ! (4,20,20,4,20,4,20,4) 
      INTEGER, KIND :: K4
      INTEGER, LEN  :: N4
      INTEGER(K4)   :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4(N5,K5)    ! (4,20,20,4,20,4,20,4,20,4) 
      INTEGER, KIND :: K5
      INTEGER, LEN  :: N5
      INTEGER(K5)   :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C817ClassIsPoly
  USE M
  IMPLICIT NONE
 
  CLASS(Level1(4,:,:,4)), POINTER :: Ptr 
  TYPE(Level1(4,20,20,4)), TARGET  :: Tar
 
  Ptr  => Tar 

  SELECT TYPE ( Ptr ) 

    CLASS IS (Level4(4,*,*,4,*,4,*,4,*,4))
      STOP 50
    CLASS IS (Level3(4,*,*,4,*,4,*,4))
      STOP 51
    CLASS IS (Level1(4,*,*,4))
      !STOP 52
      PRINT*, "OK!, Still run here" 
    CLASS IS (Level3(4,*,*,4,*,4,*,4))
      STOP 53
    CLASS IS (Level1(4,*,*,4))
      STOP 54
      
    CLASS DEFAULT
      STOP 30
  END SELECT 


  END


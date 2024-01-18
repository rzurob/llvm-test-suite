! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/CrossFeatures/SaveArr.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SaveArr.f 
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
!*  TEST CASE NAME             : SaveArr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
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
!*  Save 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,1025)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)=1
      CHARACTER(N1) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1(K2,N2)    ! (4,1025,4,20)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(K3,N3)    ! (4,1025,4,20,4,20)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N3
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM SaveArr
  USE M
  IMPLICIT NONE
  TYPE(DT(4,1025,4,20,4,20)), TARGET :: V(2,2)

  V = Fun() 
    IF (ANY(V%IArr(1) .NE. 1)) STOP 21
    IF (TRIM(V(1,1)%CArr(1)) .NE. "!") STOP 22
    IF (TRIM(V(2,2)%CArr(2)) .NE. "!") STOP 23

  CONTAINS

  RECURSIVE FUNCTION Fun()
  CLASS(DT(4,:,4,:,4,:)), SAVE, POINTER :: U(:,:)
  INTEGER :: i
  CLASS(DT(4,:,4,:,4,:)), POINTER :: Fun(:,:)

    ALLOCATE(U(2,2), SOURCE=DT(4,1025,4,20,4,20)(IArr=1))

  SELECT TYPE( U  )
  CLASS IS (DT(4,*,4,*,4,*))

    IF (ANY(U%IArr(1) .NE. 1)) STOP 31
    IF (TRIM(U(1,1)%CArr(1)) .NE. "!") STOP 32
    IF (TRIM(U(2,2)%CArr(2)) .NE. "!") STOP 33

    Fun => U

  CLASS DEFAULT
    STOP 40
  END SELECT

  END FUNCTION 

  END




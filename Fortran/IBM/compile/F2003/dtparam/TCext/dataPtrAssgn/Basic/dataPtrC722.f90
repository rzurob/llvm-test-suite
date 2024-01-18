! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC722.f
! opt variations: -qnock

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C722 (R736) A data-pointer-component-name shall be the name of a component of variable
!*  that is a data pointer.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC722
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFunc(Arg)
      INTEGER :: Arg
      INTEGER, POINTER :: IFun(:)
    END FUNCTION
  END INTERFACE

  TYPE :: DT(K1,K2,N1)    ! (4,1,3)
    INTEGER, KIND                         :: K1,K2
    INTEGER, LEN                          :: N1
    INTEGER(K1) ,                 POINTER :: Ptr(:)=>NULL()
    CHARACTER(kind=K2,len=N1), POINTER    :: CPtr
    PROCEDURE(IFunc), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(DT(4,1,3)),   TARGET  :: Arr(1)

  ASSOCIATE (As => Arr(1)%Ptr)
    AS(1:) => Arr
    As(1:1) => Arr
  END ASSOCIATE

  Arr(1)%ProcPtr(1:) => Arr(1)%Ptr
  Arr(1)%CPtr(1:) => Arr(1)%CPtr

  END



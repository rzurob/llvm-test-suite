! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/initExp/Def/InitExpDefElemLLT.f
! opt variations: -qck -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to an elemental intrinsic
!*
!*  -  LLT
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemLLT
  IMPLICIT NONE
  INTEGER :: I, J

  TYPE :: DT0(K1,K2,N1,N2)    ! (4,4,10,10)
    INTEGER, KIND            :: K1,K2
    INTEGER, LEN             :: N1,N2
    INTEGER(K1), POINTER     :: Ptr=>NULL()
    REAL(K2), ALLOCATABLE    :: R
    CHARACTER(N1)            :: C1=""
    CHARACTER(N2)            :: C2=ACHAR(1)//ACHAR(2)//ACHAR(3)//ACHAR(4)//ACHAR(5)//   &
                               ACHAR(6)//ACHAR(7)//ACHAR(8)//ACHAR(9)//ACHAR(0)
    CHARACTER(2**8-1)    :: C3=CHAR(0)
    PROCEDURE(), POINTER, NOPASS :: ProcPtr =>NULL()
  END TYPE

  TYPE(DT0(4,4,10,10)), PARAMETER :: C(16)=DT0(4,4,10,10)(R=NULL())

  TYPE :: DT(N3,K3,K4,K5,K6,K7,K8,K9)    ! (20,4,4,4,4,4,4,4)
    INTEGER, KIND :: K3,K4,K5,K6,K7,K8,K9
    INTEGER, LEN  :: N3
    LOGICAL(K3)   :: C1(16)=LLT(STRING_B=(/C%C1/), STRING_A=(/C%C2(1:0)/))
    LOGICAL(K4)   :: C2(16)=LLT(STRING_B=(/C%C1/), STRING_A=(/C%C2(10:10)/))
    LOGICAL(K5)   :: C3(16)=LLT(STRING_B=(/C%C2/), STRING_A=(/C%C2(1:9)/))
    LOGICAL(K6)   :: C4(16)=LLT(STRING_B=(/C%C2//C%C1/), STRING_A=(/C%C2(:9)//C%C2(1:0)/))
    LOGICAL(K7)   :: C5(16)=LLT(STRING_B=(/C%C2//C%C3/), STRING_A=(/C%C2(:)//C%C3(:9)/))
    LOGICAL(K8)   :: C6(16)=LLT(STRING_B=(/C%C1//C%C3/), STRING_A=(/C%C1(:)//C%C3(1:1)/))
    LOGICAL(K9)   :: C7(16)=LLT(STRING_B=(/C%C2(1:6)/),  STRING_A=(/C%C2(1:6)/))
  END TYPE

  TYPE(DT(20,4,4,4,4,4,4,4)) :: T

  IF( ANY( T%C1  .NEQV. .FALSE. ))       STOP 11
  IF( ANY( T%C2  .NEQV. .TRUE.  ))       STOP 12
  IF( ANY( T%C3  .NEQV. .FALSE. ))       STOP 13
  IF( ANY( T%C4  .NEQV. .FALSE. ))       STOP 14
  IF( ANY( T%C5  .NEQV. .FALSE. ))       STOP 15
  IF( ANY( T%C6  .NEQV. .FALSE. ))       STOP 16
  IF( ANY( T%C7  .NEQV. .FALSE. ))       STOP 17


  END



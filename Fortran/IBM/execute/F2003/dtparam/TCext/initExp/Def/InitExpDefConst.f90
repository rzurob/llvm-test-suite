! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/F2003/initExp/Def/InitExpDefConst.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefConst.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 17, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  the primary is a constant/ subobject of a constant 
!* 
!*  
!*  (ICE-318836)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpDefConst 
  IMPLICIT NONE

  TYPE :: DT(N1,K1,K2)    ! (3,4,4)
    INTEGER, KIND     :: K1,K2
    INTEGER, LEN      :: N1
    CHARACTER(N1)     :: Str="MBI"
    INTEGER(K1)       :: ID=-1
    REAL(K2), POINTER :: RPtr=>NULL()
  END TYPE
 
  INTEGER :: I, J

  TYPE(DT(3,4,4)), PARAMETER ::  C=DT(3,4,4)(), CArr(10)=(/(DT(3,4,4)(ID=i, Str="IBM"), I=1, 10)/) 

  TYPE :: DT1(K3,N2,N3)    ! (4,20,3)
    INTEGER, KIND      :: K3
    INTEGER, LEN       :: N2,N3
    TYPE(DT(N3,K3,K3)) :: Comp(10)
  END TYPE

  TYPE(DT1(4,20,3)), PARAMETER ::  C1=DT1(4,20,3)(CArr), CArr1(10)=(/(DT1(4,20,3)(CArr), I=1, 10)/) 

  TYPE, EXTENDS(DT) :: DT2(K4,N4)    ! (3,4,4,4,20)
      INTEGER, KIND :: K4
      INTEGER, LEN  :: N4
  END TYPE
 
  TYPE(DT2(3,4,4,4,20)), PARAMETER ::  C2=DT2(3,4,4,4,20)(DT=C1%Comp(1)), CArr2(10)=DT2(3,4,4,4,20)(DT=CArr1(1)%Comp(1)) 
 
 
  IF (C%Str         .NE. "MBI" ) STOP 11
  IF (C%ID          .NE. -1    ) STOP 12
 
  IF (ANY(CArr%Str  .NE. "IBM") )                       STOP 21
  IF (ANY(CArr%ID   .NE. (/(I, I=1, 10)/)) )            STOP 22
 
  IF (ANY(C1%Comp%Str          .NE. "IBM"  ))           STOP 31
  IF (ANY(C1%Comp%ID           .NE. CArr%ID))           STOP 32
 
  DO J=1, 10
    IF (ANY(CArr1(J)%Comp%Str  .NE. "IBM") )            STOP 41
    IF (ANY(CArr1(J)%Comp%ID   .NE. (/(I, I=1, 10)/)) ) STOP 42
  END DO

  IF (C2%Str          .NE. "IBM"  )           STOP 51
  IF (C2%ID           .NE. 1     )            STOP 52
 
  IF (ANY(CArr2%Str  .NE. "IBM") )            STOP 61
  IF (ANY(CArr2%ID   .NE. 1 ) )               STOP 62

  END

 

! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefElemLOG10.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemLOG10.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 12, 2006
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
!*  a reference to an elemental intrinsic
!* 
!*  -  LOG10 
!*  (319120)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(N1,K1,K2,K3)    ! (20,4,8,16)
    INTEGER, KIND :: K1,K2,K3
    INTEGER, LEN  :: N1
    REAL(K1)      :: R4=10000.0
    REAL(K2)      :: R8=100000000.0_8
    REAL(K3)      :: R6=10._16**16
  END TYPE

  TYPE(DT0(20,4,8,16)), PARAMETER :: R(128)=DT0(20,4,8,16)()

  CONTAINS

    FUNCTION ModFun(Arg1, Arg2, Arg3)
      REAL(16) :: ModFun 
      REAL(4) :: Arg1
      REAL(8) :: Arg2
      REAL(16):: Arg3

      ModFun = Arg3 + Arg2 + Arg1
    END FUNCTION

  END MODULE


  PROGRAM  InitExpDefElemLOG10
  USE M
  IMPLICIT NONE 
  INTEGER :: I, J


  INTERFACE
    FUNCTION F(Arg1, Arg2, Arg3)
      IMPORT
      REAL(INT(LOG10(R(128)%R6))) :: F
      REAL(INT(LOG10(R(128)%R4))) :: Arg1 
      REAL(INT(LOG10(R(128)%R8))) :: Arg2 
      REAL(INT(LOG10(R(128)%R6))) :: Arg3 
    END FUNCTION
  END INTERFACE

  PROCEDURE(F), POINTER :: ProcPtr

  ProcPtr => ModFun

  IF ( ProcPtr(R(1)%R4, R(2)%R8, R(3)%R6) .NE. (R(1)%R4+R(2)%R8+R(3)%R6) )  STOP 11


  END



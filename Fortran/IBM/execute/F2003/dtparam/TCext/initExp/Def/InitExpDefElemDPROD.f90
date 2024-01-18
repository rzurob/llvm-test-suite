! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemDPROD.f
! opt variations: -qnok -qnol -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemDPROD.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 07, 2006
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
!*  -  DPROD 
!*  (318967)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemDPROD
  IMPLICIT NONE 
  INTEGER :: I, J, K
 

  TYPE :: DT0(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    REAL(K1)      :: XR4(4,4) = -2.0
    REAL(K1)      :: XRR4     =  2.0
  END TYPE

  TYPE(DT0(20,4)), PARAMETER :: P=DT0(20,4)()
 
  TYPE :: DT(K2,N2)    ! (4,20) 
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
    REAL(KIND(DPROD(P%XR4, P%XRR4)))       :: TR44(4,4) = DPROD(P%XR4, P%XRR4) 
  END TYPE

  TYPE(DT(4,20)) :: T

  TYPE(DT0(20,4)), PARAMETER :: PP(4)=DT0(20,4)()
 
  TYPE :: DT1(K3,N3)    ! (4,20) 
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
    REAL(KIND(DPROD(PP(1)%XR4, PP(2)%XRR4)))    &
        :: TR44(4) = DPROD( (/(PP(I)%XR4(I,I), I=1,4)/), (/(PP(I)%XRR4, I=1,4)/))
  END TYPE

  TYPE(DT1(4,20)) :: TT


  IF ( KIND(T%TR44) .NE. 8 )    STOP 11
  IF ( ANY(T%TR44   .NE. -4 ))  STOP 12

  IF ( KIND(TT%TR44) .NE. 8 )   STOP 21
  IF ( ANY(TT%TR44   .NE. -4 )) STOP 22

  END

 

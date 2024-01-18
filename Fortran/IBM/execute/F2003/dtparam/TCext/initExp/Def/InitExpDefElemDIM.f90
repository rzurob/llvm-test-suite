! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/initExp/Def/InitExpDefElemDIM.f
! opt variations: -qnok -qnol -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemDIM.f  
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
!*  -  DIM
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemDIM
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL    :: r

  TYPE :: DT0(N1,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14)    ! (20,1,2,4,8,1,2,4,8,4,8,16,4,8,16)
    INTEGER, KIND :: K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: XI1(4,4) = -0
    INTEGER(K2)   :: XI2      = -1
    INTEGER(K3)   :: XI4(4,4) =  0
    INTEGER(K4)   :: XI8      =  1

    INTEGER(K5)   :: XII1(4,4) = 1
    INTEGER(K6)   :: XII2      = 0
    INTEGER(K7)   :: XII4(4,4) = -0
    INTEGER(K8)   :: XII8      = -1

    REAL(K9)      :: XR4(4,4) = -2.0
    REAL(K10)     :: XR8      = -0.0
    REAL(K11)     :: XR6(4,4) = +1.0

    REAL(K12)     :: XRR4(4,4) = +1.0
    REAL(K13)     :: XRR8      = -2.0
    REAL(K14)     :: XRR6(4,4) = -1.0
  END TYPE

  TYPE(DT0(20,1,2,4,8,1,2,4,8,4,8,16,4,8,16)), PARAMETER :: P=DT0(20,1,2,4,8,1,2,4,8,4,8,16,4,8,16)()
 
  TYPE :: DT(K15,N2)    ! (4,20) 
      INTEGER, KIND :: K15
      INTEGER, LEN  :: N2
    INTEGER(KIND(DIM(P%XI1, P%XII1)))   :: TI1(4,4) = DIM(P%XI1, P%XII1) 
    INTEGER(KIND(DIM(P%XI2, P%XII2)))   :: TI2(4,4) = DIM(P%XI2, P%XII2) 
    INTEGER(KIND(DIM(P%XI4, P%XII4)))   :: TI4(4,4) = DIM(P%XI4, P%XII4) 
    INTEGER(KIND(DIM(P%XI8, P%XII8)))   :: TI8(4,4) = DIM(P%XI8, P%XII8) 

    REAL(KIND(DIM(P%XR4, P%XRR4)))       :: TR4(4,4) = DIM(P%XR4, P%XRR4) 
    REAL(KIND(DIM(P%XR8, P%XRR8)))       :: TR8(4,4) = DIM(P%XR8, P%XRR8) 
    REAL(KIND(DIM(P%XR6, P%XRR6)))       :: TR6(4,4) = DIM(P%XR6, P%XRR6) 
  END TYPE

  TYPE(DT(4,20)) :: T


  IF ( KIND(T%TI1) .NE. 1 )   STOP 11
  IF ( ANY(T%TI1   .NE. 0  )) STOP 12
  IF ( KIND(T%TI2) .NE. 2 )   STOP 13
  IF ( ANY(T%TI2   .NE. 0  )) STOP 14
  IF ( KIND(T%TI4) .NE. 4 )   STOP 15
  IF ( ANY(T%TI4   .NE. 0  )) STOP 16
  IF ( KIND(T%TI8) .NE. 8 )   STOP 17
  IF ( ANY(T%TI8   .NE. 2  )) STOP 18

  IF ( KIND(T%TR4) .NE. 4 )   STOP 21
  IF ( ANY(T%TR4   .NE. 0 ))  STOP 22
  IF ( KIND(T%TR8) .NE. 8 )   STOP 23
  IF ( ANY(T%TR8   .NE. 2 ))  STOP 24
  IF ( KIND(T%TR6) .NE. 16)   STOP 25
  IF ( ANY(T%TR6   .NE. 2 ))  STOP 26

  END

 

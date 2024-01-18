! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemLOG.f
! opt variations: -ql -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemLOG.f  
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
!*  -  LOG 
!*  (319105)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemLOG
  IMPLICIT NONE 
  INTEGER :: I, J

  REAL(4),    PARAMETER :: R=2.3025851
  COMPLEX(4), PARAMETER :: Z1=(0.0000000000E+00, 3.141592741)
  COMPLEX(4), PARAMETER :: Z2=(0.0000000000E+00,-3.141592741)


  TYPE :: DT0(K1)    ! (4)
    INTEGER, KIND            :: K1
    INTEGER(K1), POINTER     :: Ptr=>NULL()
    REAL(K1), ALLOCATABLE    :: R
    REAL(KIND(LOG((/(10._4,  I=1,128)/))))  :: R4(128)=LOG((/(10._4,  I=1,128)/))
    REAL(KIND(LOG((/(10._8,  I=1,128)/))))  :: R8(128)=LOG((/(10._8,  I=1,128)/))
    REAL(KIND(LOG((/(10._16, I=1,128)/))))  :: R6(128)=LOG((/(10._16, I=1,128)/))
    PROCEDURE(), POINTER, NOPASS :: ProcPtr =>NULL()
    COMPLEX(KIND(LOG((/((-1._4, +0._4),   I=1,128)/))))  :: Z4(128)=LOG((/((-1._4, +0._4),  I=1,128)/))
    COMPLEX(KIND(LOG((/((-1._8, +0._8),   I=1,128)/))))  :: Z8(128)=LOG((/((-1._8, +0._8),  I=1,128)/))
    COMPLEX(KIND(LOG((/((-1._16, +0._16), I=1,128)/))))  :: Z6(128)=LOG((/((-1._16,-0._16), I=1,128)/))
  END TYPE

  TYPE(DT0(4)), PARAMETER :: C=DT0(4)(R=NULL(), Ptr=NULL(), ProcPtr=NULL()) 

  TYPE :: DT(K2)    ! (4)
    INTEGER, KIND :: K2
    LOGICAL(K2)   :: L1=ALL(ABS(LOG((/10./))-R) .GT. 1.E-6 )
    LOGICAL(K2)   :: L2=ALL(ABS(LOG((/(-1._8, +0._8)/))-Z1) .GT. 1.E-6 )
  END TYPE

  TYPE(DT(4)) :: T

  IF ( T%L1 )  STOP 8
  IF ( T%L2 )  STOP 9

  IF (KIND(C%R4)  .NE. 4)    STOP 11
  IF (KIND(C%R8)  .NE. 8)    STOP 12
  IF (KIND(C%R6)  .NE. 16)   STOP 13
  IF (KIND(C%Z4)  .NE. 4)    STOP 14
  IF (KIND(C%Z8)  .NE. 8)    STOP 15
  IF (KIND(C%Z6)  .NE. 16)   STOP 16
 
  IF( ANY( ABS(C%R4-R)  .GE. 1.E-6 ))       STOP 21
  IF( ANY( ABS(C%R8-R)  .GE. 1.E-6 ))       STOP 22
  IF( ANY( ABS(C%R6-R)  .GE. 1.E-6 ))       STOP 23
  IF( ANY( ABS(C%Z4-Z1) .GE. 1.E-6 ))       STOP 24
  IF( ANY( ABS(C%Z8-Z1) .GE. 1.E-6 ))       STOP 25
! IF( ANY( ABS(C%Z6-Z2) .GE. 1.E-6 ))       STOP 26 ! COMPLEX(16) is an IBM extension
                                                    ! we do not have the same behavior as COMPLEX(8)
  IF( ANY( ABS(C%Z6+Z2) .GE. 1.E-6 ))       STOP 26


  END


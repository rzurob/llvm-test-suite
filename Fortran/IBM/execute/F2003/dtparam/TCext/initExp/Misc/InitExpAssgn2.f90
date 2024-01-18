! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/initExp/Misc/InitExpAssgn2.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn2.f  
!*  TESTOP CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept. 07 2006
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Charber 289074 
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
!*  Init with Null 
!* 
!* (325078)
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(X1,X2,X3,X4,X5,N1)    ! (1,2,4,8,16,1)
    INTEGER, KIND          :: X1,X2,X3,X4,X5
    INTEGER, LEN           :: N1
    INTEGER(X1), POINTER   :: I1(:)
    INTEGER(X2), POINTER   :: I2(:)
    INTEGER(X3), POINTER   :: I4(:)
    INTEGER(X4), POINTER   :: I8(:)

    REAL(X3), POINTER      :: R4(:)
    REAL(X4), POINTER      :: R8(:)
    REAL(X5), POINTER      :: R6(:)

    COMPLEX(X3), POINTER   :: Z4(:)
    COMPLEX(X4), POINTER   :: Z8(:)
    COMPLEX(X5), POINTER   :: Z6(:)
 
    CHARACTER(N1), POINTER :: C(:)
  END TYPE

  END MODULE

  PROGRAM InitExpAssgn2 
  USE M
  IMPLICIT NONE

  INTEGER     :: I, J, K

  TYPE :: DT(X6,N2,X7,X8,X9,X10,N3)    ! (4,20,1,2,8,16,1)
    INTEGER, KIND                 :: X6,X7,X8,X9,X10
    INTEGER, LEN                  :: N2,N3
    TYPE(DT0(X7,X8,X6,X9,X10,N3)) :: T1=DT0(X7,X8,X6,X9,X10,1)(      &
                 I1=NULL(),  &  
                 I2=NULL(),  &  
                 I4=NULL(),  &  
                 I8=NULL(),  &  
                 R4=NULL(),  &  
                 R8=NULL(),  &  
                 R6=NULL(),  &  
                 Z4=NULL(),  &  
                 Z8=NULL(),  &  
                 Z6=NULL(),  &  
                 C =NULL()   &  
                       )
  END TYPE

 
  TYPE (DT(4,20,1,2,8,16,1)) :: T(128)=[(DT(4,20,1,2,8,16,1)(T1=DT0(1,2,4,8,16,1)( &
                 I1=NULL(),  &
                 I2=NULL(),  &
                 I4=NULL(),  &
                 I8=NULL(),  &
                 R4=NULL(),  &
                 R8=NULL(),  &
                 R6=NULL(),  &
                 Z4=NULL(),  &
                 Z8=NULL(),  &
                 Z6=NULL(),  &
                 C =NULL()  )&
                 ), I=0,127)]



  DO I=1, 128
    IF (  ASSOCIATED(T(I)%T1%I1 )) STOP 11
    IF (  ASSOCIATED(T(I)%T1%I2 )) STOP 12
    IF (  ASSOCIATED(T(I)%T1%I4 )) STOP 13
    IF (  ASSOCIATED(T(I)%T1%I8 )) STOP 14

    IF (  ASSOCIATED(T(I)%T1%R4 )) STOP 21
    IF (  ASSOCIATED(T(I)%T1%R8 )) STOP 22
    IF (  ASSOCIATED(T(I)%T1%R6 )) STOP 23

    IF (  ASSOCIATED(T(I)%T1%Z4 )) STOP 31
    IF (  ASSOCIATED(T(I)%T1%Z8 )) STOP 32
    IF (  ASSOCIATED(T(I)%T1%Z6 )) STOP 33

    IF (  ASSOCIATED(T(I)%T1%C  )) STOP 41
  END DO


  END 


 

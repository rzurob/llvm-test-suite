! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/initExp/Def/InitExpDefAIMAG.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefAIMAG.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 22, 2006
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
!*  - AIMAG (Z) /ENUM
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  ENUM, BIND(C)
    ENUMERATOR :: Zero 
    ENUMERATOR :: One 
    ENUMERATOR :: Two 
  END ENUM

  COMPLEX, PARAMETER :: Z=(Zero, One+Two*Two)
 
  ENUM, BIND(C)
    ENUMERATOR :: X2=INT(AIMAG((One,Two))) 
    ENUMERATOR :: X4=INT(AIMAG((One,Two))+AIMAG((One,Two))) 
    ENUMERATOR :: X3=INT(AIMAG((X4, One+X2))) 
    ENUMERATOR :: X10=INT(AIMAG(2*Z)) 
  END ENUM
 
  END MODULE


  PROGRAM InitExpDefElemACOS 
  USE M
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT1(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    COMPLEX(K1)   :: Z(One+Two*Two)=(/((AIMAG(Z),AIMAG(Z)), I=One,X2+X3)/)
  END TYPE

  TYPE(DT1(20,4)) :: T1
  TYPE(DT1(20,4)) :: Arr1(One:INT(AIMAG(Z))) 
  PARAMETER  (  Arr1=(/(DT1(20,4)(Z=(/Z,Z,Z,Z,Z/)), I=1,5)/) )

 
  IF (X2             .NE. 2   )                       STOP 11
  IF (X3             .NE. 3   )                       STOP 12
  IF (X4             .NE. 4   )                       STOP 13
  IF (X10            .NE. 10  )                       STOP 14


  IF (ANY(LBOUND(T1%Z)   .NE. (/1/)) )                STOP 21
  IF (ANY(UBOUND(T1%Z)   .NE. (/5/)) )                STOP 22
  !IF (ANY(T1%Z          .NE. (/((AIMAG(Z),AIMAG(Z)), I=One,X2+X3)/)) )     STOP 23
  IF (ANY(T1%Z           .NE. (/((AIMAG(Z),AIMAG(Z)), I=1,5)/)) )           STOP 24


  IF (ANY(LBOUND(Arr1)   .NE. (/1/)  ))                STOP 31
  IF (ANY(UBOUND(Arr1)   .NE. (/5/) ))                 STOP 32
  DO I=1, 5
    IF (ANY(Arr1(I)%Z    .NE. Z ))                     STOP 33
  END DO

  END

 

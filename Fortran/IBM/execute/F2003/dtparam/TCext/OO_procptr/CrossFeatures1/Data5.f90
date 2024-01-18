! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Data5.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Data5.f 
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
!*  TEST CASE NAME             : Data5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 13, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : Feature 289058 
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
!*  Initialize proc-ptr in block data.
!* 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      SEQUENCE
      INTEGER(K1)   :: Id
      PROCEDURE(), POINTER, NOPASS :: ProcPtr 
    END TYPE
  END MODULE


  PROGRAM Data5 
  USE M
  IMPLICIT NONE 

  INTEGER :: I

  TYPE (DT(20,4)) :: V(3)
  TYPE (DT(20,4)) :: U(3)
  TYPE (DT(20,4)) :: W(3)


  COMMON /B1/V
! COMMON /B2/U
  COMMON /B3/W


  IF (ASSOCIATED(V(1)%ProcPtr))   STOP 11
  IF (ASSOCIATED(V(2)%ProcPtr))   STOP 11
  IF (ASSOCIATED(V(3)%ProcPtr))   STOP 11
  IF (ANY(V%Id .NE. 1))           STOP 12
 
! IF (ASSOCIATED(U(1)%ProcPtr))   STOP 21
! IF (ASSOCIATED(U(2)%ProcPtr))   STOP 21
! IF (ASSOCIATED(U(3)%ProcPtr))   STOP 21
! IF (ANY(U%Id .NE. 2))           STOP 22
 
  IF (ASSOCIATED(W(1)%ProcPtr))   STOP 31
  IF (ASSOCIATED(W(2)%ProcPtr))   STOP 32
  IF (ASSOCIATED(W(3)%ProcPtr))   STOP 33
  IF (ANY(W%Id .NE. 3))           STOP 34
   
  END

  BLOCK DATA
  USE M

  TYPE (DT(20,4)) :: V(3)
! TYPE (DT) :: U(3)
  TYPE (DT(20,4)) :: W(3)

  COMMON /B1/V
  DATA V /3*DT(20,4)(1, NULL())/  

! COMMON /B2/U                         
! DATA (U(I)%Id, I=1,3 ) /3*2/  
! DATA (U(I)%ProcPtr, I=1,3 ) /3*NULL()/   !not allowed

  COMMON /B3/W
  DATA (W(I), I=1,3,2 ) /2*DT(20,4)(3, NULL())/  
  DATA (W(I), I=2,3,2 ) /1*DT(20,4)(3, NULL())/  

  END BLOCK DATA




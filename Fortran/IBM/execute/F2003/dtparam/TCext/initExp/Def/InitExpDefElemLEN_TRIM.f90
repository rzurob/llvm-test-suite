! GB DTP extension using:
! ftcx_dtp -qk -qreuse=self /tstdev/F2003/initExp/Def/InitExpDefElemLEN_TRIM.f
! opt variations: -qck -qnok -qreuse=none

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemLEN_TRIM.f  
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
!*  -  LEN_TRIM 
!*  (318833)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefElemLEN_TRIM
!  IMPLICIT INTEGER(KIND(LEN_TRIM(C%C1)))(A) 
  IMPLICIT INTEGER(KIND(LEN_TRIM("   ")))(A) 
  INTEGER :: I, J

  TYPE :: DT(K1,N1)    ! (4,10)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C1=""
    CHARACTER(N1) :: C2=" 123     "
    CHARACTER(N1) :: C3="          "
    CHARACTER(N1) :: C4="123456789"//CHAR(0)
  END TYPE

  TYPE(DT(4,10)), PARAMETER :: C(128)=DT(4,10)() 

  INTEGER(KIND(LEN_TRIM(C%C1))), PARAMETER :: TC1(128) = LEN_TRIM(C%C1)
  INTEGER(KIND(LEN_TRIM(C%C2))), PARAMETER :: TC2(128) = LEN_TRIM(C%C2)
  INTEGER(KIND(LEN_TRIM(C%C3))), PARAMETER :: TC3(128) = LEN_TRIM(C%C3)
  INTEGER(KIND(LEN_TRIM(C%C4))), PARAMETER :: TC4(128) = LEN_TRIM(C%C4)

  INTEGER(KIND(LEN_TRIM(STRING=C%C1, KIND=1))), PARAMETER :: TCI1(128) = LEN_TRIM(STRING=C%C1, KIND=1)
  INTEGER(KIND(LEN_TRIM(STRING=C%C2, KIND=2))), PARAMETER :: TCI2(128) = LEN_TRIM(STRING=C%C2, KIND=2)
  INTEGER(KIND(LEN_TRIM(STRING=C%C3, KIND=4))), PARAMETER :: TCI4(128) = LEN_TRIM(STRING=C%C3, KIND=4)
  INTEGER(KIND(LEN_TRIM(STRING=C%C4, KIND=8))), PARAMETER :: TCI8(128) = LEN_TRIM(STRING=C%C4, KIND=8)

  IF( KIND(AAA) .NE. 4 )         STOP 10

  IF( KIND(TC1) .NE. 4 )         STOP 11
  IF( ANY( TC1  .NE. 0  ))       STOP 12
  IF( KIND(TC2) .NE. 4 )         STOP 13
  IF( ANY( TC2  .NE. 4  ))       STOP 14
  IF( KIND(TC3) .NE. 4 )         STOP 15
  IF( ANY( TC3  .NE. 0  ))       STOP 16
  IF( KIND(TC4) .NE. 4 )         STOP 17
  IF( ANY( TC4  .NE. 10 ))       STOP 18

  IF( KIND(TCI1) .NE. 1 )         STOP 21
  IF( ANY( TCI1  .NE. 0  ))       STOP 22
  IF( KIND(TCI2) .NE. 2 )         STOP 23
  IF( ANY( TCI2  .NE. 4  ))       STOP 24
  IF( KIND(TCI4) .NE. 4 )         STOP 25
  IF( ANY( TCI4  .NE. 0  ))       STOP 26
  IF( KIND(TCI8) .NE. 8 )         STOP 27
  IF( ANY( TCI8  .NE. 10 ))       STOP 28


  END



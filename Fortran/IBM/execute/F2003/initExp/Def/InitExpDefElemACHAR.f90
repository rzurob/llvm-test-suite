!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemACHAR.f  
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
!*  -ACHAR
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemACHAR 
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT
    CHARACTER(ICHAR(ACHAR(3)))  :: C=ACHAR(3)//ACHAR(3)//ACHAR(3)
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER(LEN(ACHAR(3)//ACHAR(3)//ACHAR(3)))  :: C1(3)=(/(ACHAR(I), I=1,3)/)
  END TYPE

  TYPE(DT)  :: T 
  TYPE(DT1) :: T1  

  CHARACTER(LEN=SIZE((/(ACHAR(I), I=1,3)/))) :: &
         C(SIZE((/(ACHAR(3), I=1,3)/)):SIZE((/(ACHAR(3), I=1,5)/))) =(/(ACHAR(I), I=1,3)/)
 
  TYPE(DT1), PARAMETER :: Const=DT1(DT=DT(),C1=ACHAR(0)//ACHAR(0)//ACHAR(0))

 
  IF (T%C       .NE. ACHAR(3)//ACHAR(3)//ACHAR(3)   )          STOP 11
  IF (LEN(T%C)  .NE. 3   )                                     STOP 11

  IF (T1%C       .NE. ACHAR(3)//ACHAR(3)//ACHAR(3)   )         STOP 21
  IF (LEN(T%C)   .NE. 3   )                                    STOP 22
  IF (ANY(T1%C1      .NE. (/(ACHAR(I), I=1,3)/))   )           STOP 23
  IF (LEN(T1%C1) .NE. 3   )                                    STOP 24

  IF (ANY(LBOUND(C)   .NE. (/3/)) )                            STOP 31
  IF (ANY(UBOUND(C)   .NE. (/5/)) )                            STOP 32
  IF (ANY(C   .NE. (/(ACHAR(I), I=1,3)/)) )                    STOP 33

  IF (Const%C   .NE. ACHAR(3)//ACHAR(3)//ACHAR(3) )            STOP 41
  IF (ANY(Const%C1  .NE. ACHAR(0)//ACHAR(0)//ACHAR(0)) )       STOP 42



  END

 

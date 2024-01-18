! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/initExp/Def/InitExpDefADJUSTL.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefADJUSTL.f  
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
!*  - ADJUSTL
!*  (318833)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefADJUSTL 
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    CHARACTER*(LEN(ADJUSTL("   ")))  :: C=ADJUSTL(" 12")
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (4)
    CHARACTER*(LEN(ADJUSTL("12 ")))  :: C1(3)=(/(ADJUSTL(CHAR(I)), I=1,3)/)
  END TYPE

  TYPE(DT(4))  :: T 
  TYPE(DT1(4)) :: T1  

  CHARACTER*(LEN(ADJUSTL("012"))) :: &
         C(LEN(ADJUSTL("012")):LEN(ADJUSTL("     "))) =(/(ADJUSTL(CHAR(I)), I=1,3)/)
 
  TYPE(DT1(4)), PARAMETER :: Const(LEN(ADJUSTL(" 123 "))) =   &
                          (/(DT1(4)(C=ADJUSTL("123"),C1=ADJUSTL("321")), I=1,5)/)

 
  IF (T%C       .NE. "12 "   )                                 STOP 11
  IF (LEN(T%C)  .NE. 3   )                                     STOP 12

  IF (T1%C       .NE. "12 "   )                                STOP 21
  IF (LEN(T%C)   .NE. 3   )                                    STOP 22
  IF ( ANY(T1%C1 .NE. (/(ADJUSTL(CHAR(I)), I=1,3)/) ) )        STOP 23
  IF (LEN(T1%C1) .NE. 3   )                                    STOP 24

  IF (ANY(LBOUND(C)   .NE. (/3/)) )                            STOP 31
  IF (ANY(UBOUND(C)   .NE. (/5/)) )                            STOP 32
  IF (ANY(C   .NE. (/(ADJUSTL(CHAR(I)), I=1,3)/)) )            STOP 33

  IF (SIZE(Const)   .NE. 5 )                                   STOP 40
  IF (ANY(Const%C   .NE. (/(ADJUSTL("123"), I=1,5)/)) )        STOP 41
  DO I=1, 5
    IF (ANY(Const(I)%C1  .NE. (/(ADJUSTL("321"), I=1,3)/)) )   STOP 42
  END DO

  END

 

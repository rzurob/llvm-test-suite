!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpACOSD.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug 16, 2006
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
!*  a reference to ACOSD  -- IBM extension
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpACOSD 
  IMPLICIT NONE
  INTEGER :: I, J, K

  TYPE :: DT
    CHARACTER*(INT(ACOSD(0.5)))  :: C=CHAR(INT(ACOSD(0.5)))
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER*(INT(ACOSD(0.5)))  :: C1(3)=(/(CHAR(INT(ACOSD(0.5))), I=1,3)/)
  END TYPE

  TYPE(DT)  :: T 
  TYPE(DT1) :: T1  

  CHARACTER*(INT(ACOSD(0.5))) :: &
         C(INT(ACOSD(0.5)):INT(ACOSD(0.5))) =(/CHAR(INT(ACOSD(0.5)))/)
 
  TYPE(DT1), PARAMETER :: Const(INT(ACOSD(0.5))) =   &
                          DT1(C1=(/(CHAR(INT(ACOSD(0.5))), I=1,3)/), C=CHAR(INT(ACOSD(0.5))))

  CHARACTER(59), PARAMETER :: Spaces=" "

  IF (T%C       .NE. ACHAR(60)//Spaces  )            STOP 11
  IF (LEN(T%C)  .NE. 60   )                          STOP 12

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           STOP 13
  IF (LEN(T%C)   .NE. 60   )                         STOP 14
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         STOP 15
  IF (LEN(T1%C1) .NE. 60   )                         STOP 16

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 STOP 17
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 STOP 18
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          STOP 19

  IF (SIZE(Const)   .NE. 60 )                        STOP 20
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        STOP 21
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   STOP 22
  END DO


  CALL IntSub8()
  CALL IntSub16()

  CONTAINS

  SUBROUTINE IntSub8()
  TYPE :: DT
    CHARACTER*(INT(ACOSD(0.5_8)))  :: C=CHAR(INT(ACOSD(0.5_8)))
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER*(INT(ACOSD(0.5_8)))  :: C1(3)=(/(CHAR(INT(ACOSD(0.5_8))), I=1,3)/)
  END TYPE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1

  CHARACTER*(INT(ACOSD(0.5_8))) :: &
         C(INT(ACOSD(0.5_8)):INT(ACOSD(0.5_8))) =(/CHAR(INT(ACOSD(0.5_8)))/)

  TYPE(DT1), PARAMETER :: Const(INT(ACOSD(0.5_8))) =   &
                          DT1(C1=(/(CHAR(INT(ACOSD(0.5_8))), I=1,3)/), C=CHAR(INT(ACOSD(0.5_8))))

  CHARACTER(59), PARAMETER :: Spaces=" "

  IF (T%C       .NE. ACHAR(60)//Spaces  )            STOP 31
  IF (LEN(T%C)  .NE. 60   )                          STOP 32

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           STOP 33
  IF (LEN(T%C)   .NE. 60   )                         STOP 34
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         STOP 35
  IF (LEN(T1%C1) .NE. 60   )                         STOP 36

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 STOP 37
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 STOP 38
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          STOP 39

  IF (SIZE(Const)   .NE. 60 )                        STOP 40
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        STOP 41
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   STOP 42
  END DO

  END SUBROUTINE


  SUBROUTINE IntSub16()
  TYPE :: DT
    CHARACTER*(INT(ACOSD(0.5_16)))  :: C=CHAR(INT(ACOSD(0.5_16)))
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
    CHARACTER*(INT(ACOSD(0.5_16)))  :: C1(3)=(/(CHAR(INT(ACOSD(0.5_16))), I=1,3)/)
  END TYPE

  TYPE(DT)  :: T
  TYPE(DT1) :: T1

  CHARACTER*(INT(ACOSD(0.5_16))) :: &
         C(INT(ACOSD(0.5_16)):INT(ACOSD(0.5_16))) =(/CHAR(INT(ACOSD(0.5_16)))/)

  TYPE(DT1), PARAMETER :: Const(INT(ACOSD(0.5_16))) =   &
                          DT1(C1=(/(CHAR(INT(ACOSD(0.5_16))), I=1,3)/), C=CHAR(INT(ACOSD(0.5_16))))

  CHARACTER(59), PARAMETER :: Spaces=" "

  IF (T%C       .NE. ACHAR(60)//Spaces  )            STOP 51
  IF (LEN(T%C)  .NE. 60   )                          STOP 52

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           STOP 53
  IF (LEN(T%C)   .NE. 60   )                         STOP 54
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         STOP 55
  IF (LEN(T1%C1) .NE. 60   )                         STOP 56

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 STOP 57
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 STOP 58
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          STOP 59

  IF (SIZE(Const)   .NE. 60 )                        STOP 60
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        STOP 61
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   STOP 62
  END DO

  END SUBROUTINE




  END

 

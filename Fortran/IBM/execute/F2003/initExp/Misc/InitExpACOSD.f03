!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  a reference to ACOSD  -- IBM extension
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

  IF (T%C       .NE. ACHAR(60)//Spaces  )            ERROR STOP 11
  IF (LEN(T%C)  .NE. 60   )                          ERROR STOP 12

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           ERROR STOP 13
  IF (LEN(T%C)   .NE. 60   )                         ERROR STOP 14
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         ERROR STOP 15
  IF (LEN(T1%C1) .NE. 60   )                         ERROR STOP 16

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 ERROR STOP 17
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 ERROR STOP 18
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          ERROR STOP 19

  IF (SIZE(Const)   .NE. 60 )                        ERROR STOP 20
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        ERROR STOP 21
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   ERROR STOP 22
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

  IF (T%C       .NE. ACHAR(60)//Spaces  )            ERROR STOP 31
  IF (LEN(T%C)  .NE. 60   )                          ERROR STOP 32

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           ERROR STOP 33
  IF (LEN(T%C)   .NE. 60   )                         ERROR STOP 34
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         ERROR STOP 35
  IF (LEN(T1%C1) .NE. 60   )                         ERROR STOP 36

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 ERROR STOP 37
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 ERROR STOP 38
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          ERROR STOP 39

  IF (SIZE(Const)   .NE. 60 )                        ERROR STOP 40
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        ERROR STOP 41
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   ERROR STOP 42
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

  IF (T%C       .NE. ACHAR(60)//Spaces  )            ERROR STOP 51
  IF (LEN(T%C)  .NE. 60   )                          ERROR STOP 52

  IF (T1%C       .NE. ACHAR(60)//Spaces  )           ERROR STOP 53
  IF (LEN(T%C)   .NE. 60   )                         ERROR STOP 54
  IF ( ANY(T1%C1 .NE. ACHAR(60)//Spaces  ) )         ERROR STOP 55
  IF (LEN(T1%C1) .NE. 60   )                         ERROR STOP 56

  IF (ANY(LBOUND(C)   .NE. (/60/)) )                 ERROR STOP 57
  IF (ANY(UBOUND(C)   .NE. (/60/)) )                 ERROR STOP 58
  IF (ANY(C   .NE.    ACHAR(60)//Spaces ) )          ERROR STOP 59

  IF (SIZE(Const)   .NE. 60 )                        ERROR STOP 60
  IF (ANY(Const%C   .NE. ACHAR(60)//Spaces) )        ERROR STOP 61
  DO I=1,  3
    IF (ANY(Const(I)%C1  .NE. ACHAR(60)//Spaces) )   ERROR STOP 62
  END DO

  END SUBROUTINE




  END



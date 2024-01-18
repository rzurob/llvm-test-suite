! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Forall.f 
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
!*  TEST CASE NAME             : Forall 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
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
!* Forall 
!* ()
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Forall 
  IMPLICIT CLASS(DT)(U)
  TYPE :: DT 
    INTEGER :: Int
    CHARACTER(3) :: C 
  END TYPE
  INTEGER :: i
 
  CALL Sub((/(DT(Int=-1, C="123"), i=1,16)/))

  CONTAINS

  SUBROUTINE Sub(U)
  DIMENSION :: U(:)

  SELECT TYPE (U)
  CLASS IS (DT)

    IF (ANY(U%Int   .NE. -1))      STOP 20
    IF (ANY(U%C     .NE. "123"))   STOP 21
    IF (ANY(SHAPE(U).NE. (/16/)))  STOP 22
   
    FORALL  (I=1:16 )
      U%Int = 1
      U%C="321"
    END FORALL 

    IF (ANY(U%Int .NE. 1))     STOP 30
    IF (ANY(U%C   .NE. "321")) STOP 31

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END




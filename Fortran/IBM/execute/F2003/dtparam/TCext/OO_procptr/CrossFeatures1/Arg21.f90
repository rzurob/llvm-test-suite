! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Arg21.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnock

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg21.f 
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
!*  TEST CASE NAME             : Arg21.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 26, 2005
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
!*  Argument association - Implicit interface
!*  If the dummy argument is referenced as a subroutine, the actual argumenti
!*  shall be  a subroutine, subroutine procedure pointer, or dummy procedure.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base(1,3))(P)

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE
 
    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base(1,*)), INTENT(IN)  :: Arg2 
        TYPE(Base(1,*)), INTENT(OUT) :: Arg1 
      END SUBROUTINE 
    END INTERFACE

 
  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base(1,*)), INTENT(IN)  :: Arg2 
  TYPE(Base(1,*)), INTENT(OUT) :: Arg1 
    Arg1 = Arg2
  END SUBROUTINE 


  PROGRAM Arg20
  USE M
  IMPLICIT TYPE(Base(1,3))(P)

  CALL IntSub1()
  CALL IntSub2()


  CONTAINS

  SUBROUTINE IntSub1() 
  PROCEDURE(IntF) :: ExtSub 
    CALL IntSub3(ExtSub, ExtSub) 
  END SUBROUTINE 

  SUBROUTINE IntSub2() 
  PROCEDURE(IntF) :: ExtSub 
    CALL IntSub4(ExtSub) 
  END SUBROUTINE 

  SUBROUTINE IntSub3(Proc0, Proc1)
  IMPLICIT TYPE(Base(1,3))(P)

  PROCEDURE(IntF) :: Proc0
  PROCEDURE(IntF) :: Proc1
  TYPE(Base(1,3)) :: V

  CALL Proc0(V, Base(1,3)("321"))
  IF (V%C .NE. "321") STOP 15

  CALL Proc1(V, Base(1,3)("123"))
  IF (V%C .NE. "123") STOP 13

  END SUBROUTINE

  SUBROUTINE IntSub4(Proc)
  IMPLICIT TYPE(Base(1,3))(P)
  PROCEDURE(IntF) :: Proc
  TYPE(Base(1,3)) :: V

  CALL Proc(V, Base(1,3)("321"))
  IF (V%C .NE. "321") STOP 15

  END SUBROUTINE

  END


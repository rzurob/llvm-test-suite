! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: StrComp5.f 
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
!*  TEST CASE NAME             : StrComp5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 18, 2005
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
!*  Structure component - Structure constructor 
!*  (304726) (307073)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      PROCEDURE(), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      TYPE(Base) :: BComp
    END TYPE

    INTERFACE
      PURE SUBROUTINE Sub(Arg1, Arg2 )
        IMPORT 
        TYPE(DT), INTENT(OUT):: Arg1 
        PROCEDURE() :: Arg2 
      END SUBROUTINE 
 
      PURE SUBROUTINE Sub1(Arg1, Arg2 )
        IMPORT 
        TYPE(Base),   INTENT(OUT) :: Arg1 
        PROCEDURE()  :: Arg2 
      END SUBROUTINE
    END INTERFACE

  END MODULE

  PURE SUBROUTINE ToD(Arg1, Arg2 )
  USE M
  TYPE(DT), INTENT(OUT):: Arg1 
  INTERFACE 
     PURE SUBROUTINE S()
     END SUBROUTINE
  END INTERFACE 
  PROCEDURE(S) :: Arg2 
    Arg1%BComp%ProcPtr => Arg2
  END SUBROUTINE 
 
  PURE SUBROUTINE ToB(Arg1, Arg2 )
  USE M 
  TYPE(Base),   INTENT(OUT) :: Arg1 
  INTERFACE 
     PURE SUBROUTINE S()
     END SUBROUTINE
  END INTERFACE 
  PROCEDURE(S)  :: Arg2 
    Arg1%ProcPtr => Arg2
  END SUBROUTINE 
 
  PROGRAM StrComp5  
  USE M
  IMPLICIT NONE 

  PROCEDURE(Sub)  :: ToD 
  PROCEDURE(Sub1) :: ToB 
  TYPE(DT), PARAMETER :: Para=DT(Base(NULL()))

  PROCEDURE(),  POINTER :: ProcPtr 

  TYPE(DT) :: V(3)=Para, U
  INTEGER  :: I

  DO I=1, 3
    IF ( ASSOCIATED(V(I)%BComp%ProcPtr) ) STOP 14
    V(I)%BComp%ProcPtr  => ToB 
    IF ( .NOT. ASSOCIATED(V(I)%BComp%ProcPtr, ToB) ) STOP 24
    U%BComp = Base(NULL())
    CALL V(I)%BComp%ProcPtr(U%BComp, V(I)%BComp%ProcPtr)
    IF ( .NOT. ASSOCIATED(U%BComp%ProcPtr, ToB) )    STOP 25
  END DO

  ProcPtr => ToD
  V(3)=Para

  DO I=1, 3
    CALL  ProcPtr(V(I), ProcPtr)
    IF ( .NOT. ASSOCIATED(V(I)%BComp%ProcPtr, ProcPtr) )  STOP 34
    U = DT(Base(NULL()))
    CALL V(I)%BComp%ProcPtr(U, V(I)%BComp%ProcPtr)
    IF ( .NOT. ASSOCIATED(U%BComp%ProcPtr, ToD) )  STOP 35
  END DO


  END



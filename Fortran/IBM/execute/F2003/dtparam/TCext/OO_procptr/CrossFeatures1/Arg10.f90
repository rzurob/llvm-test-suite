! GB DTP extension using:
! ftcx_dtp -qk -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/Arg10.f
! opt variations: -qck -qnok -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg10.f 
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
!*  TEST CASE NAME             : Arg10.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 23, 2005
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
!*  Explicit dummy procedure - Characteristics 
!*  Bound
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(N1)             :: C
      TYPE(Base(K1,:)), POINTER :: BPtr 
    END TYPE
 
    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(4,*)) :: Arg 
        TYPE(Base(4,3)):: IntF(2:4) 
      END FUNCTION
    END INTERFACE
 
    INTERFACE
      FUNCTION IntF1(Arg)
      IMPORT
        TYPE(Base(4,*)) :: Arg 
        TYPE(Base(4,3)):: IntF1(3) 
      END FUNCTION
    END INTERFACE
 
  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4,*)) :: Arg 
  TYPE(Base(4,3)) :: ExtFun(3) 
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg10
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun 
  PROCEDURE(IntF1), POINTER :: ProcPtr 

  CALL IntSub(ExtFun)

  ProcPtr => ExtFun
  CALL IntSub1(ProcPtr)

  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT TYPE(Base(4,3))(A)
    PROCEDURE(IntF1) :: Arg
    TYPE(Base(4,3)) :: V(3)
    TYPE(Base(4,3)), TARGET :: Tar=Base(4,3)("abc", NULL()) 
      V = Arg(Base(4,3)("123", Tar))
      IF (ANY(V%C .NE. "123"))              STOP 11
      IF (.NOT. ASSOCIATED(V(2)%BPtr, Tar)) STOP 12
      IF (V(1)%BPtr%C .NE. "abc"  )         STOP 13
      IF (V(2)%BPtr%C .NE. "abc"  )         STOP 14
      IF (V(3)%BPtr%C .NE. "abc"  )         STOP 15
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT TYPE(Base(4,3))(A)
    PROCEDURE(IntF), POINTER :: Arg
    TYPE(Base(4,3)) :: V(3)
    TYPE(Base(4,3)), TARGET :: Tar=Base(4,3)("abc", NULL()) 
      V = Arg(Base(4,3)("123", Tar))
      IF (ANY(V%C .NE. "123"))              STOP 21
      IF (.NOT. ASSOCIATED(V(2)%BPtr, Tar)) STOP 22
      IF (V(1)%BPtr%C .NE. "abc"  )         STOP 23
      IF (V(2)%BPtr%C .NE. "abc"  )         STOP 24
      IF (V(3)%BPtr%C .NE. "abc"  )         STOP 25
    END SUBROUTINE


  END


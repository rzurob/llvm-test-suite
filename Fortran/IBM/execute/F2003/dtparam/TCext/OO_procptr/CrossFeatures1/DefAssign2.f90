! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/OO_procptr/CrossFeatures1/DefAssign2.f
! opt variations: -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: DefAssign2.f
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
!*  TEST CASE NAME             : DefAssign2.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 17, 2005
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
!*  Defined assignment - where 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 

  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToC
      END FUNCTION
    END INTERFACE

    TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(N2,K2)    ! (20,4)
      INTEGER, KIND              :: K2
      INTEGER, LEN               :: N2
      INTEGER(K2)                :: Id=0
      TYPE(Base(K2,N2)), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
      MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
      MODULE PROCEDURE MyAssign4
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    ELEMENTAL SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(Base(4,*)), INTENT(OUT) :: Arg1
    TYPE(Base(4,*)), INTENT(IN)  :: Arg2
      Arg1%ProcPtr => Arg2%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign2 (Arg1, arg2)
    TYPE(DT(*,4)),    INTENT(OUT) :: Arg1
    TYPE(Base(4,*)),  INTENT(IN)  :: Arg2
      Arg1%Id = -1
      ALLOCATE(Arg1%BComp)
      Arg1%BComp%ProcPtr => Arg2%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign3 (Arg1, Arg2)
    TYPE(Base(4,*)), INTENT(OUT) :: Arg1
    TYPE(DT(*,4)),   INTENT(IN)  :: Arg2
      Arg1%ProcPtr => Arg2%BComp%ProcPtr
    END SUBROUTINE

    ELEMENTAL SUBROUTINE MyAssign4 (Arg1, Arg2)
    TYPE(DT(*,4)),  INTENT(OUT) :: Arg1
    TYPE(DT(*,4)),  INTENT(IN)  :: Arg2
      ALLOCATE(Arg1%BComp)
      Arg1%BComp%ProcPtr => Arg2%BComp%ProcPtr
      Arg1%Id = -Arg2%Id
    END SUBROUTINE


  END MODULE
 

  PROGRAM DefAssign2 
  USE M
  IMPLICIT NONE 

  INTEGER :: I
  TYPE(Base(4,20)) :: B1(511), B2(511)
  TYPE(DT(20,4))   :: D1(511), D2(511)
  TYPE(Base(4,20)), TARGET :: BTar

  WHERE ((/(.TRUE., I=1,511)/)  )
    B1 = Base(4,20)(Fun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(B1(I)%ProcPtr, Fun)) STOP 11
  END DO

  WHERE ((/(.TRUE., I=1,511)/)  )
    D1 = Base(4,20)(Fun)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D1(I)%BComp%ProcPtr, Fun)) STOP 22
    IF (D1(I)%Id .NE. -1) STOP 23   
  END DO

  BTar = Base(4,20)(RetPtr(Fun))
  WHERE ((/(.TRUE., I=1,511)/)  )
    B2 = DT(20,4)(-1, BTar)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(B2(I)%ProcPtr, Fun)) STOP 32
  END DO

  BTar = Base(4,20)(Fun)
  WHERE ((/(.TRUE., I=1,511)/)  )
    D2 = DT(20,4)(-3, BTar)
  END WHERE
  DO I=1, 511
    IF (.NOT. ASSOCIATED(D2(I)%BComp%ProcPtr, Fun)) STOP 42
    IF (D2(I)%Id .NE. 3) STOP 43   
  END DO

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg 
    RetPtr => Arg 
  END FUNCTION

  END


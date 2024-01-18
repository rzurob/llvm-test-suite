! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Misc16.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Misc16.f 
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
!*  TEST CASE NAME             :  Misc16.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 09, 2005
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
!*  
!*  Enum 
!*  
!*  
!*  (Mem Fault)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  IMPLICIT INTEGER(P)

  ENUM, BIND(C)
    ENUMERATOR :: Mon
    ENUMERATOR :: Tue 
    ENUMERATOR :: Wed 
    ENUMERATOR :: Thu 
    ENUMERATOR :: Fri 
    ENUMERATOR :: Sat 
    ENUMERATOR :: Sun 
  END ENUM

  ENUM, BIND(C)
    ENUMERATOR :: One 
    ENUMERATOR :: Two 
    ENUMERATOR :: Thr 
  END ENUM


  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    SEQUENCE
    INTEGER(K1)   :: ID=One + Mon
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: ModFun(SIZE(Arg))
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  TYPE(DT(4)) :: Arg(:)
  TYPE(DT(4)) :: ModFun1(SIZE(Arg))
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  PROGRAM Misc16
  USE M
  IMPLICIT INTEGER(P)

  PROCEDURE(ModFun1), POINTER :: ProcPtr
  TYPE(DT(4))                   :: U(10000)

  IF ( ANY(U%ID .NE. 0 )) STOP 11

  U = DT(4)( Thr, NULL() )

  U(1:6) = U((/Tue, Wed, Thu, Fri, Sat, Sun /) )
  IF ( ANY(U((/Tue, Wed, Thu, Fri, Sat, Sun /))%ID .NE. 2 )) STOP 12
   
  ProcPtr => ModFun1
  U = ProcPtr( (/(DT(4)(I, ModFun), I=1, 10000) /) )

  DO I  = 1, 10000
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Modfun) ) STOP 22
    IF ( U(I)%Id .NE. I )                         STOP 23
  END DO

  END



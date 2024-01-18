! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Arg1.f
! opt variations: -qck -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Arg1.f 
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
!*  TEST CASE NAME             : Arg1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 19, 2005
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
!*  Dummy argument is a procedure pointer - function/Null 
!*  (304727)(305366)(305719) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE
  
  END MODULE

  MODULE M
  USE M0

    TYPE  :: DT(K2,N2)    ! (4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      PROCEDURE(TYPE(Base(4,3))), NOPASS, POINTER :: ProcPtr
    END TYPE
   
    CONTAINS
 
    FUNCTION ModFun(Arg)
    TYPE(Base(4,*)) :: Arg
    TYPE(Base(4,20)) :: ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(ModFun), POINTER :: Fun
  PROCEDURE(ModFun), POINTER :: RetPtr 
    RetPtr => Fun 
  END FUNCTION
 
  PROGRAM Arg1 
  USE M
  IMPLICIT NONE 

  INTERFACE
    FUNCTION RetPtr(Fun)
      IMPORT 
      PROCEDURE(ModFun), POINTER :: Fun
      PROCEDURE(ModFun), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  PROCEDURE(ModFun), POINTER :: ProcPtr

  ProcPtr => Modfun
  CALL IntSub(NULL())
  CALL IntSub(NULL(ProcPtr))
  CALL IntSub(RetPtr(NULL()))

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(ModFun), POINTER :: Ptr
    IF ( ASSOCIATED(Ptr)) STOP 41
  END SUBROUTINE 

  END


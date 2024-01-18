! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/TypeDecl.f
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: TypeDecl.f 
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
!*  TEST CASE NAME             :  TypeDecl.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 07, 2005
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
!*  Should not have EXTERNAL or INTRINSIC attribute
!*  specified unless it is a function 
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Save 
  IMPLICIT TYPE(Base(4,3))(P)

  TYPE :: Base(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C
  END TYPE

  PROCEDURE(TYPE(Base(4,3))), POINTER :: ProcPtr => NULL()

  CALL IntSub1(ProcPtr)
  CALL IntSub2(ProcPtr)
  CALL IntSub3(ProcPtr)
  CALL IntSub4(ProcPtr)
  CALL IntSub5(ProcPtr)

  CONTAINS

    SUBROUTINE IntSub1(Proc)
    TYPE(Base(4,3))  :: Proc
    PROCEDURE(TYPE(Base(4,3))) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub2(Proc)
    TYPE(Base(4,3))  :: Proc
    PROCEDURE() :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub3(Proc)
    TYPE(Base(4,3))                     :: Proc
    PROCEDURE(TYPE(Base(4,:))), POINTER :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub4(ProcPtr)
    TYPE(Base(4,3))                     :: ProcPtr
    PROCEDURE(TYPE(Base(4,:))), POINTER :: ProcPtr
    END SUBROUTINE

    SUBROUTINE IntSub5(ProcPtr)
    IMPLICIT NONE
    PROCEDURE(), POINTER :: ProcPtr
    TYPE(Base(4,3))      :: ProcPtr
    END SUBROUTINE


  END



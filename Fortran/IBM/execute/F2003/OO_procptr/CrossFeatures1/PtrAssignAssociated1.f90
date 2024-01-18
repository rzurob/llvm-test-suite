! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignAssociated1.f 
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
!*  TEST CASE NAME             : PtrAssignAssociated1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 18, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*  The procedure pointer's status, proc ptr in common 
!*  (304454) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  
  CONTAINS 
    FUNCTION ModFun(Arg)
    CHARACTER(4)          :: Arg(:)
    CHARACTER(4), POINTER :: ModFun(:) 
      !ALLOCATE(ModFun(SIZE(Arg)), SOURCE=Arg)  ! not 10.1
      ALLOCATE(ModFun(SIZE(Arg)))
      ModFun = Arg
    END FUNCTION

  END MODULE


  PROGRAM PtrAssignAssociated1 
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun), POINTER :: ProcPtr
  CHARACTER(4),      POINTER :: CPtr
 
  INTERFACE 
    SUBROUTINE ExtSub(Proc) 
      IMPORT
      PROCEDURE(ModFun) :: Proc 
    END SUBROUTINE
  END INTERFACE

  COMMON /CB/ProcPtr, CPtr
 
  !ALLOCATE(CPtr, SOURCE="4321") ! not 10.1
  ALLOCATE(CPtr)
  CPtr = "4321"

  ProcPtr => ModFun
  CALL ExtSub(ProcPtr)

  END

  SUBROUTINE ExtSub(Proc) 
  USE M
  IMPLICIT NONE
  PROCEDURE(ModFun)          :: Proc 
  PROCEDURE(ModFun), POINTER :: ProcPtr 
  CHARACTER(4),      POINTER :: CPtr
  INTEGER :: i
  COMMON /CB/ProcPtr, CPtr

  IF ( .NOT. ASSOCIATED(CPtr) )  STOP 11
  IF ( CPtr .NE. "4321" )        STOP 12
 
  IF ( .NOT. ASSOCIATED(ProcPtr) )          STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ModFun) )  STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr, Proc) )    STOP 23

  ASSOCIATE ( As => RESHAPE(ProcPtr((/( "IBM!", i=1,256)/)), (/16,16 /) )) 
    IF ( ANY( SHAPE(As) .NE. (/16,16/) ))   STOP 31
    IF ( ANY( As        .NE. "IBM!" ))      STOP 32
  END ASSOCIATE

  END SUBROUTINE


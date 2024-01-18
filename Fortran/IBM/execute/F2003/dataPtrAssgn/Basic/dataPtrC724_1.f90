!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC724_1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 03, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  C724 (R739) An expr shall be a reference to a function whose result is a data pointer. 
!*
!*  (323066)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC724_1
  IMPLICIT NONE
  
  INTEGER              :: I, J
  LOGICAL(1)           :: L(0:1023) = .TRUE.
  COMPLEX, TARGET      :: C(-1:1023) = (1.0, -1.0) 

  TYPE :: DT
    INTEGER :: ID
  END TYPE

  CLASS(DT), POINTER   :: PtrDT(:)

  CLASS(*), POINTER    :: Ptr(:)

  ! Shall not query the characheristics of function result ??

  Ptr(LBOUND(L,1):) => F1(L,  LBOUND(L,1))
  IF (ANY(LBOUND(Ptr) .NE. (/0    /) )) STOP 11
  IF (ANY(UBOUND(Ptr) .NE. (/1023 /) )) STOP 12
  SELECT TYPE (Ptr)
  TYPE IS (LOGICAL(1))
     IF (ANY(Ptr .NEQV. .TRUE. ))       STOP 13
  CLASS DEFAULT
    STOP 14
  END SELECT
 
  !Ptr(LBOUND(F2(C,  LBOUND(C,1)),1):UBOUND(F2(C, LBOUND(C,1)),1)) => F2(C, LBOUND(C,1))
  Ptr(LBOUND(C,1):UBOUND(C,1)) => F2(C, LBOUND(C,1))
  IF (ANY(LBOUND(Ptr) .NE. (/-1   /) )) STOP 21
  IF (ANY(UBOUND(Ptr) .NE. (/1023 /) )) STOP 22
  SELECT TYPE (Ptr)
  TYPE IS (COMPLEX)
     IF (ANY(Ptr .NE. (1.0, -1.0) ))    STOP 23
  CLASS DEFAULT
    STOP 24
  END SELECT
  IF ( .NOT. ASSOCIATED(Ptr, C))        STOP 25

  ALLOCATE(PtrDT(1:513), SOURCE=DT(-1)) 
  Ptr(LBOUND(F3(PtrDT), 1):UBOUND(F3(PtrDT), 1)) => F3(PtrDT) 
  IF (ANY(LBOUND(Ptr) .NE. (/1    /) )) STOP 31
  IF (ANY(UBOUND(Ptr) .NE. (/513  /) )) STOP 32
  SELECT TYPE (Ptr)
  TYPE IS (DT)
     IF (ANY(Ptr%ID .NE. -1 ))          STOP 33
  CLASS DEFAULT
    STOP 34
  END SELECT
  IF ( .NOT. ASSOCIATED(Ptr, PtrDT))    STOP 35

  CONTAINS

  FUNCTION F1(Arg, LB)
  CLASS(*) :: Arg(:)
  INTEGER  :: LB
  CLASS(*), POINTER :: F1(:)
    ALLOCATE(F1(LB:LB-1+SIZE(Arg)), SOURCE=Arg) 
  END FUNCTION
 
  FUNCTION F2(Arg, LB)
  INTEGER           :: LB
  CLASS(*), TARGET  :: Arg(LB:)
  CLASS(*), POINTER :: F2(:)
    F2(LBOUND(Arg,1):) => Arg 
  END FUNCTION
 
  FUNCTION F3(Arg)
  CLASS(DT), POINTER :: Arg(:)
  CLASS(DT), POINTER :: F3(:)
    F3(LBOUND(Arg,1):UBOUND(Arg,1)) => Arg 
  END FUNCTION
 
  END



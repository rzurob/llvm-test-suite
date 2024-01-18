!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTypBndDefAssgn.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 31, 2006
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
!*  
!* Interaction with type bound generic
!* 
!*  -- Defined assignment 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE Mod

  TYPE :: DT
    CHARACTER(8) :: ID
    CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    PROCEDURE, PASS(ARG2)  :: ModSub
  END TYPE

  CONTAINS

  ELEMENTAL SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT), INTENT(INOUT) :: Arg1 
  CLASS(DT), INTENT(IN)    :: Arg2 
    Arg1%ID = "ModSub-"//Arg2%ID 
  END SUBROUTINE 


  END MODULE

  PROGRAM dataPtrTypBndDefAssgn 
  USE Mod
  IMPLICIT NONE

  INTEGER            :: I, J, K
  INTEGER, PARAMETER :: N=10, M=100

  TYPE(DT), TARGET  :: Tar1(M)=(/(DT(ID=CHAR(I)), I=M,1,-1)/)
  TYPE(DT), TARGET  :: Tar2(N,N)=RESHAPE((/(DT(ID=CHAR(I)), I=1,M)/),(/N,N/))
  TYPE(DT),POINTER  :: Ptr(:, :)


  Ptr(1:,1:) => Tar2
  IF (.NOT. ASSOCIATED(Ptr, Tar2))                                            STOP 21
  IF (ANY( LBOUND(Ptr) .NE. (/1, 1 /)))                                       STOP 22
  IF (ANY( UBOUND(Ptr) .NE. (/N,N/)))                                         STOP 23
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=1,M)/),(/N,N/))))           STOP 24

  Ptr = Tar2 
  IF (ANY( Ptr%ID  .NE. RESHAPE((/("ModSub-" //CHAR(I), I=1,M)/),(/N,N/))))   STOP 25

  Ptr(0:8,0:8) => Tar1
  IF (.NOT. ASSOCIATED(Ptr))                                                  STOP 31
  IF (ANY( LBOUND(Ptr) .NE. (/0, 0 /)))                                       STOP 32
  IF (ANY( UBOUND(Ptr) .NE. (/8,8/)))                                         STOP 33
  IF (ANY( Ptr%ID      .NE. RESHAPE((/(CHAR(I), I=M,19,-1)/),(/N-1,N-1/))))   STOP 34

  Ptr = Ptr
  IF (ANY( Ptr%ID  .NE. RESHAPE((/("ModSub-" //CHAR(I), I=M,20,-1)/),(/N-1,N-1/))))  STOP 35
  IF (ANY( Tar1(1:81)%ID  .NE. (/("ModSub-" //CHAR(I), I=M,20,-1)/)))                STOP 36
  IF (ANY( Tar1(82:)%ID   .NE. (/(CHAR(I), I=19,1,-1)/)))                            STOP 36
  END



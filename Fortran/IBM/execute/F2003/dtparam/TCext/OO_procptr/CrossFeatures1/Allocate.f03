! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_procptr/CrossFeatures1/Allocate.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The allocate stmt
!*
!*  (304081)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id
      PROCEDURE(INTEGER), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    abstract interface
        function proc (arg)
        import
            TYPE (DT(20,4)) :: proc
            TYPE(DT(*,4))  :: Arg
        end function
    end interface

    CONTAINS

    FUNCTION ModFun()
    INTEGER ModFun
      Modfun = -1
    END FUNCTION

  END MODULE

  PROGRAM Allocate
  USE M
  IMPLICIT NONE

  procedure(proc), pointer :: ProcPtr=>NULL()
!  PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcPtr=>NULL()
  TYPE ( DT(20,4) ),         POINTER :: V
!  PROCEDURE(TYPE(DT(20,4)))          :: Fun
  procedure(proc) :: fun

  ProcPtr => Fun

  !ALLOCATE(V, SOURCE=ProcPtr(DT(-1, ModFun))) ! not 10.1
  ALLOCATE(V)
  V = ProcPtr(DT(20,4)(-1, ModFun))
  IF ( .NOT. ASSOCIATED(V) )                 ERROR STOP 11
  IF ( V%Id .NE. -1 )                        ERROR STOP 12
  IF ( .NOT. ASSOCIATED(V%ProcPtr, ModFun) ) ERROR STOP 13

  DEALLOCATE(V)

  END

  FUNCTION Fun(Arg)
  USE M
  TYPE (DT(20,4)) :: Fun
  TYPE(DT(*,4))  :: Arg

    if (arg%n1 /= 20) error stop 100
    Fun = Arg
  END FUNCTION

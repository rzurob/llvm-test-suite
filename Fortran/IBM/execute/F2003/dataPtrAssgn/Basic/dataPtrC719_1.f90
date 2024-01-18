!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C719 (R735) If bounds-remapping-list is specified, the number of bounds-remappings
!*  shall equal the rank of data-pointer-object.
!*
!*  -20 Dimensions
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER :: ID
  CONTAINS
    PROCEDURE :: GetID
  END TYPE

  CONTAINS
    ELEMENTAL FUNCTION GetID(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER   :: GetID
      GetID = Arg%ID
    END FUNCTION
  END MODULE

  PROGRAM dataPtrC719_1
  USE M
  IMPLICIT NONE

  TYPE(DT),   TARGET  ::     T(0:0,0:0)=DT(1)
  TYPE(DT),   TARGET  ::     T1(0:0)=DT(1)
  CLASS(DT ), POINTER :: Ptr20(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)


  Ptr20(0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0) => T(:,0)
  IF (ANY(LBOUND(Ptr20) .NE. (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /))) STOP 11
  IF (ANY(UBOUND(Ptr20) .NE. (/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /))) STOP 12
  IF (ANY(Ptr20%GetId() .NE. 1))                                            STOP 13

  T1 = DT(-1)
  Ptr20(0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1,0:0,1:1) => T1
  IF (ANY(LBOUND(Ptr20) .NE. (/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1 /))) STOP 21
  IF (ANY(UBOUND(Ptr20) .NE. (/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1 /))) STOP 22
  IF (ANY(Ptr20%GetId() .NE. -1))                                           STOP 23

  Ptr20(0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,0:0,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) => T(0,:)
  IF (ANY(LBOUND(Ptr20) .NE. (/0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1 /))) STOP 31
  IF (ANY(UBOUND(Ptr20) .NE. (/0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1 /))) STOP 32
  IF (ANY(Ptr20%GetId() .NE. 1))                                            STOP 33

  END



! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC717_3.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 2, 2006
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
!*  C717 (R735) If data-target is unlimited polymorphic, data-pointer-object shall be
!*  unlimited polymorphic,  of a sequence derived type, or of a type with
!*  the BIND attribute.
!*  -- bind(c)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM dataPtrC717_3
  USE ISO_C_BINDING
  IMPLICIT NONE

  INTEGER :: I, J

  TYPE, BIND(C) :: DT0
    INTEGER(C_INT) :: I= -1
  END TYPE

  TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
    CLASS(*), POINTER :: CPtr(:)
  END TYPE

  TYPE(DT(4,20))           :: T
  TYPE(DT0), TARGET  :: Arr(0:2)=(/DT0(1), DT0(2), DT0(3)/)
  CLASS(*), POINTER  :: CPtr(:)
  TYPE(DT0), POINTER :: CPtr1(:)

  CPtr(LBOUND(Arr,1):) => Arr
  T%CPtr(LBOUND(Arr,1):) => CPtr
  IF (ANY(LBOUND(T%CPtr) .NE. (/0/)))   ERROR STOP 11
  IF (ANY(UBOUND(T%CPtr) .NE. (/2/)))   ERROR STOP 12
  CPtr1 => T%CPtr
  IF (ANY(CPtr1%I .NE. (/1,2,3/) ))     ERROR STOP 13


  ALLOCATE(CPtr(0:2), SOURCE=(/DT0(3),DT0(2),DT0(1)/))
  I=LBOUND(CPtr,1); J=UBOUND(CPtr,1)-1
  T%CPtr(I:J) => CPtr(I:J+1)
  IF (ANY(LBOUND(T%CPtr) .NE. (/I/)))   ERROR STOP 21
  IF (ANY(UBOUND(T%CPtr) .NE. (/J/)))   ERROR STOP 22

  CPtr1(I:) => T%CPTr
  IF (ANY(CPtr1%I        .NE. (/3,2/))) ERROR STOP 23

  T%CPtr(1:-1) => T%CPtr
  IF (ANY(LBOUND(T%CPtr) .NE. (/1/)))   ERROR STOP 31
  IF (ANY(UBOUND(T%CPtr) .NE. (/0/)))   ERROR STOP 32

  END



! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/OO_poly/selectType/Misc/Misc9.f
! opt variations: -qnok -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  Complain  :
!*  " (S) Associate name fun is not associated with a variable or it is associated
!*    with a variable with a vector subscript.  It must not be redefined or
!*    become undefined."
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc9


  TYPE :: Base(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
  END TYPE

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base(4,*)) :: Arg
  CLASS(Base(4,20)), POINTER :: Fun
    ALLOCATE(Fun)
    SELECT TYPE( Fun )
      TYPE IS (Base(4,*))
        Fun=Arg
    END SELECT
  END FUNCTION

  END

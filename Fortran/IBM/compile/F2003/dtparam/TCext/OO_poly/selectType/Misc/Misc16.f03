! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Misc/Misc16.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 16, 2005
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
!*
!*  ICE-TYPE GUARD outside of select type construct
!*  (298437)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc16

  type :: DT(k1,n1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: n1
  end type
  ASSOCIATE (V=>10)
    TYPE IS (DT(4,*))
  END ASSOCIATE

  END



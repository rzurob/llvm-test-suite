!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint10zd
!*
!*  DATE                       : 2006-08-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier with incorrect KIND in assignment (COMPLEX)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type, incorrect kind, COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic type specifiers enforce correct KIND values.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we verify that COMPLEX is correctly handled.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10zd

  implicit none
  complex :: zarr(1)

  zarr  = (/complex(kind=32):: (1.2,2.3)/)
  zarr  = (/complex(kind=-1):: (1.2,2.3)/)
  zarr  = (/complex(kind=3):: (1.2,2.3)/)
  zarr  = (/complex(kind=9):: (1.2,2.3)/)
  zarr  = (/complex(kind=100):: (1.2,2.3)/)
  zarr  = (/complex(kind='4'):: (1.2,2.3)/)
  zarr  = (/complex(kind=4.4):: (1.2,2.3)/)
  zarr  = (/complex(kind=(4.4,8.8)):: (1.2,2.3)/)
  zarr  = (/complex(kind=.true.):: (1.2,2.3)/)
  zarr  = (/complex(kind=.false.):: (1.2,2.3)/)

  zarr  = (/complex(32):: (1.2,2.3)/)
  zarr  = (/complex(-1):: (1.2,2.3)/)
  zarr  = (/complex(3):: (1.2,2.3)/)
  zarr  = (/complex(9):: (1.2,2.3)/)
  zarr  = (/complex(100):: (1.2,2.3)/)
  zarr  = (/complex('4'):: (1.2,2.3)/)
  zarr  = (/complex(4.4):: (1.2,2.3)/)
  zarr  = (/complex((4.4,8.8)):: (1.2,2.3)/)
  zarr  = (/complex(.true.):: (1.2,2.3)/)
  zarr  = (/complex(.false.):: (1.2,2.3)/)

end program acetint10zd

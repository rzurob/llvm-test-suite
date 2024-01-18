!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint10
!*
!*  DATE                       : 2006-08-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier in assignment
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that the array constructor using just intrinsic type names works
!*  correctly within an assignment statement.  Use different forms of type
!*  specifier, with and without "kind=".  The test is successful if the programme
!*  compiles correctly.
!*  Parts of this test seem similar to acetint01, but these test different forms
!*  of assignment, not use as an actual argument.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*  Here we test all intrinsics types but CHARACTER.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint10

  implicit none

  integer     :: iarr(3)
  integer(1)  :: iar1(3)
  integer(2)  :: iar2(3)
  integer(4)  :: iar4(3)
  integer(8)  :: iar8(3)

  real        :: rarr(3)
  real(4)     :: rar4(3)
  real(8)     :: rar8(3)
  real(16)    :: ra16(3)

  double precision :: darr(3)

  complex     :: zarr(1)
  complex(4)  :: zar4(1)
  complex(8)  :: zar8(1)
  complex(16) :: za16(1)

  logical     :: larr(2)
  logical(1)  :: lar1(2)
  logical(2)  :: lar2(2)
  logical(4)  :: lar4(2)
  logical(8)  :: lar8(2)

  ! Integer:
  iarr  = (/integer         :: 1,2,3/)

  iar1  = (/integer(kind=1) :: 1,2,3/)
  iar2  = (/integer(kind=2) :: 1,2,3/)
  iar4  = (/integer(kind=4) :: 1,2,3/)
  iar8  = (/integer(kind=8) :: 1,2,3/)

  iar1  = (/integer(1)      :: 1,2,3/)
  iar2  = (/integer(2)      :: 1,2,3/)
  iar4  = (/integer(4)      :: 1,2,3/)
  iar8  = (/integer(8)      :: 1,2,3/)

  ! Real:
  rarr  = (/real            :: 1.2,2.3,3.4/)

  rar4  = (/real(kind=4)    :: 1.2,2.3,3.4/)
  rar8  = (/real(kind=8)    :: 1.2,2.3,3.4/)
  ra16  = (/real(kind=16)   :: 1.2,2.3,3.4/)

  rar4  = (/real(4)         :: 1.2,2.3,3.4/)
  rar8  = (/real(8)         :: 1.2,2.3,3.4/)
  ra16  = (/real(16)        :: 1.2,2.3,3.4/)

  darr  = (/double precision:: 1.2,2.3,3.4/)

  ! Complex:
  zarr  = (/complex         :: (1.2,2.3)/)

  zar4  = (/complex(kind=4) :: (1.2,2.3)/)
  zar8  = (/complex(kind=8) :: (1.2,2.3)/)
  za16  = (/complex(kind=16)::(1.2,2.3)/)

  zar4  = (/complex(4)      :: (1.2,2.3)/)
  zar8  = (/complex(8)      :: (1.2,2.3)/)
  za16  = (/complex(16)     ::(1.2,2.3)/)


  ! Logical:
  larr  = (/logical         :: .true.,.false./)

  lar1  = (/logical(kind=1) :: .false.,.true./)
  lar2  = (/logical(kind=2) :: .true.,.false./)
  lar4  = (/logical(kind=4) :: .false.,.true./)
  lar8  = (/logical(kind=8) :: .false.,.true./)

  lar1  = (/logical(1)      :: .false.,.true./)
  lar2  = (/logical(2)      :: .true.,.false./)
  lar4  = (/logical(4)      :: .false.,.true./)
  lar8  = (/logical(8)      :: .false.,.true./)

end program acetint10

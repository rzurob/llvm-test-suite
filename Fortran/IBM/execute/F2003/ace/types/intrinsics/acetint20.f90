!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME             : acetint20
!*
!*  DATE                       : 2006-06-08
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Intrinsic type specifier in assignment
!*                               - just numeric and logical
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
!*  specifier, with and without "kind=".  Owing to the wide range of forms
!*  allowed for characters, they are tested separately.
!*  The test is successful if the programme compiles correctly.
!*  Parts of this test seem similar to acetint01, but these test different forms
!*  of assignment, not use as an actual argument.
!*  We look at the correctness of the value and that of the dynamic type and
!*  kind of the constructed array in a separate test.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint20

  implicit none

  integer :: iarr(3)
  real    :: rarr(3)
  double precision :: darr(3)
  complex :: zarr(1)
  logical :: larr(2)
  character :: charr(3)
  integer, parameter :: BYTE   =  1
  integer, parameter :: SHORT  =  2
  integer, parameter :: INT    =  4
  integer, parameter :: LONG   =  8
  integer, parameter :: LONGER = 16

  integer(1), parameter :: SHORT1 = 2
  integer(2), parameter :: SHORT2 = 2
  integer(4), parameter :: SHORT4 = 2
  integer(8), parameter :: SHORT8 = 2

  ! Integer:
  iarr  = (/integer(kind=1):: 1,2,3/)
  iarr  = (/integer(kind=2):: 1,2,3/)
  iarr  = (/integer(kind=4):: 1,2,3/)
  iarr  = (/integer(kind=8):: 1,2,3/)

  iarr  = (/integer(kind=BYTE):: 1,2,3/)
  iarr  = (/integer(kind=SHORT):: 1,2,3/)
  iarr  = (/integer(kind=INT):: 1,2,3/)
  iarr  = (/integer(kind=LONG):: 1,2,3/)

  iarr  = (/integer(kind=SHORT1):: 1,2,3/)
  iarr  = (/integer(kind=SHORT2):: 1,2,3/)
  iarr  = (/integer(kind=SHORT4):: 1,2,3/)
  iarr  = (/integer(kind=SHORT8):: 1,2,3/)

  iarr  = (/integer(kind=2*BYTE):: 1,2,3/)
  iarr  = (/integer(kind=SHORT+2):: 1,2,3/)
  iarr  = (/integer(kind=INT-(2*BYTE)):: 1,2,3/)
  iarr  = (/integer(kind=LONG/2):: 1,2,3/)

  iarr  = (/integer(1):: 1,2,3/)
  iarr  = (/integer(2):: 1,2,3/)
  iarr  = (/integer(4):: 1,2,3/)
  iarr  = (/integer(8):: 1,2,3/)

  iarr  = (/integer(BYTE):: 1,2,3/)
  iarr  = (/integer(SHORT):: 1,2,3/)
  iarr  = (/integer(INT):: 1,2,3/)
  iarr  = (/integer(LONG):: 1,2,3/)

  iarr  = (/integer(SHORT1):: 1,2,3/)
  iarr  = (/integer(SHORT2):: 1,2,3/)
  iarr  = (/integer(SHORT4):: 1,2,3/)
  iarr  = (/integer(SHORT8):: 1,2,3/)

  iarr  = (/integer(2*BYTE):: 1,2,3/)
  iarr  = (/integer(SHORT+2):: 1,2,3/)
  iarr  = (/integer(INT-(2*BYTE)):: 1,2,3/)
  iarr  = (/integer(LONG/2):: 1,2,3/)

  iarr  = (/integer   :: 1,2,3/)

  ! Real:
  rarr  = (/real(kind=4) :: 1.2,2.3,3.4/)
  rarr  = (/real(kind=8) :: 1.2,2.3,3.4/)
  rarr  = (/real(kind=16):: 1.2,2.3,3.4/)
  rarr  = (/real(kind=LONGER):: 1.2,2.3,3.4/)
  rarr  = (/real(kind=LONGER-LONG):: 1.2,2.3,3.4/)

  rarr  = (/real(4) :: 1.2,2.3,3.4/)
  rarr  = (/real(8) :: 1.2,2.3,3.4/)
  rarr  = (/real(16):: 1.2,2.3,3.4/)
  rarr  = (/real(LONGER):: 1.2,2.3,3.4/)
  rarr  = (/real(LONGER-LONG):: 1.2,2.3,3.4/)

  rarr  = (/real    :: 1.2,2.3,3.4/)

  ! No kind allowed here:
  darr  = (/double precision:: 1.2,2.3,3.4/)

  ! Complex:
  zarr  = (/complex(kind=4):: (1.2,2.3)/)
  zarr  = (/complex(kind=8):: (1.2,2.3)/)
  zarr  = (/complex(kind=16)::(1.2,2.3)/)
  zarr  = (/complex(kind=LONGER)::(1.2,2.3)/)

  zarr  = (/complex(4):: (1.2,2.3)/)
  zarr  = (/complex(8):: (1.2,2.3)/)
  zarr  = (/complex(16)::(1.2,2.3)/)
  zarr  = (/complex(LONGER)::(1.2,2.3)/)

  zarr  = (/complex   :: (1.2,2.3)/)

  ! Logical:
  larr  = (/logical(kind=1):: .false.,.true./)
  larr  = (/logical(kind=2):: .true.,.false./)
  larr  = (/logical(kind=4):: .false.,.true./)
  larr  = (/logical(kind=8):: .false.,.true./)
  larr  = (/logical(kind=LONG):: .false.,.true./)

  larr  = (/logical(1):: .false.,.true./)
  larr  = (/logical(2):: .true.,.false./)
  larr  = (/logical(4):: .false.,.true./)
  larr  = (/logical(8):: .false.,.true./)
  larr  = (/logical(LONG):: .false.,.true./)

  larr  = (/logical   :: .true.,.false./)

end program acetint20

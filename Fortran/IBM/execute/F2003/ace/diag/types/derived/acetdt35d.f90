!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetdt35d
!*
!*  DATE                       : 2006-11-13
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : AC's in static initialisation of allocatable/pointer components
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : static, AC, initialisation, initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Diagnostic: Verify that references to non-"null()" values in static
!*  initialisation expressions for AC's of derived types with allocatable
!*  or pointer components are flagged.  Also verify that these same expressions
!*  are acceptable in a simple assignment statement.  This is a diagnostic
!*  counterpart to ace/types/derived/acetdt35.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetdt35dmod

  implicit none

  type Contained
     double precision :: dp
     double complex   :: dz
  end type Contained

  type ADerived
     integer, allocatable :: iafield (:)
     complex, allocatable :: zafield
     type(Contained), allocatable :: dafield
     type(Contained), allocatable :: daArray(:)
     real, pointer :: rpfield
     character, pointer :: cpfield(:)
     type(Contained), pointer :: dpfield
     type(Contained), pointer :: dpArray(:)
  end type ADerived

end module acetdt35dmod


program acetdt35d

  use acetdt35dmod
  implicit none

  integer, parameter :: iParmArray(2) = [1,2]
  complex, parameter :: zParm = (1.234d-2,2.134d2)
  type (Contained), parameter :: dParm = Contained(5.6789d1,zParm**2)
  type (Contained), parameter :: dParmArray(2) = [Contained:: Contained(5.6789d1,zParm**2),Contained(5.6789d1,zParm**2)]

  real, target       :: rt1 = 1.2
  character, target  :: ct(2) = ['a','b']
  type (Contained), target :: dt = Contained(1.1d1,(2.2d2,3.3d3))
  type (Contained), target :: dtArray(2) = [Contained:: Contained(4.4d4,(5.5d5,6.6d6)), Contained(7.7d7,(8.8d8,9.9d9))]

  integer :: i

  ! allocatable with initialisation:
  type (Contained), allocatable  :: cArr(:) = [Contained(2d0,(3d0,4d0))]

  ! types with allocatable/pointer components:
  ! Verify that an all-null constructor is acceptable:
  type (ADerived) :: adArray0(1) = [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),null(),null())]

  ! Now test incorrect usage:
  type (ADerived) :: adArray1(1) = [ADerived:: ADerived(iParmArray,null(),null(),null(),null(),null(),null(),null())]
  type (ADerived) :: adArray2(1) = [ADerived:: ADerived(null(),zParm,null(),null(),null(),null(),null(),null())]
  type (ADerived) :: adArray3(1) = [ADerived:: ADerived(null(),null(),dParm,null(),null(),null(),null(),null())]
  type (ADerived) :: adArray4(1) = [ADerived:: ADerived(null(),null(),null(),dParmArray,null(),null(),null(),null())]
  type (ADerived) :: adArray5(1) = [ADerived:: ADerived(null(),null(),null(),null(),rt1,null(),null(),null())]
  type (ADerived) :: adArray6(1) = [ADerived:: ADerived(null(),null(),null(),null(),null(),ct,null(),null())]
  type (ADerived) :: adArray7(1) = [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),dt,null())]
  type (ADerived) :: adArray8(1) = [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),null(),dtArray)]

  type (ADerived) :: adArrayc1(1) = [ADerived:: ADerived([1,2],null(),null(),null(),null(),null(),null(),null())]
  type (ADerived) :: adArrayc2(1) = [ADerived:: ADerived(null(),(1.,2.),null(),null(),null(),null(),null(),null())]
  type (ADerived) :: adArrayc3(1) = [ADerived:: ADerived(null(),null(),Contained(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null())]
  type (ADerived) :: adArrayc4(1) = [ADerived:: ADerived(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null())]

  ! And repeat with implied-do:
  type (ADerived) :: adArrayI1(1) = [ADerived:: (ADerived(iParmArray,null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayI2(1) = [ADerived:: (ADerived(null(),zParm,null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayI3(1) = [ADerived:: (ADerived(null(),null(),dParm,null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayI4(1) = [ADerived:: (ADerived(null(),null(),null(),dParmArray,null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayI5(1) = [ADerived:: (ADerived(null(),null(),null(),null(),rt1,null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayI6(1) = [ADerived:: (ADerived(null(),null(),null(),null(),null(),ct,null(),null()), i=1,1)]
  type (ADerived) :: adArrayI7(1) = [ADerived:: (ADerived(null(),null(),null(),null(),null(),null(),dt,null()), i=1,1)]
  type (ADerived) :: adArrayI8(1) = [ADerived:: (ADerived(null(),null(),null(),null(),null(),null(),null(),dtArray), i=1,1)]

  type (ADerived) :: adArrayIc1(1) = [ADerived:: (ADerived([1,2],null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayIc2(1) = [ADerived:: (ADerived(null(),(1.,2.),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayIc3(1) = [ADerived:: (ADerived(null(),null(),Contained(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived) :: adArrayIc4(1) = [ADerived:: (ADerived(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null()), i=1,1)]

  ! Verify that the AC's are acceptable in the right context:

  adArray0 = [ADerived:: ADerived(iParmArray,null(),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),zParm,null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),dParm,null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),dParmArray,null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),null(),rt1,null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),null(),null(),ct,null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),dt,null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),null(),null(),null(),null(),dtArray)]

  adArray0 = [ADerived:: ADerived([1,2],null(),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),(1.,2.),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),Contained(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null())]

  adArray0 = [ADerived:: (ADerived(iParmArray,null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),zParm,null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),dParm,null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),dParmArray,null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),null(),rt1,null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),null(),null(),ct,null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),null(),null(),null(),dt,null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),null(),null(),null(),null(),dtArray), i=1,1)]

  adArray0 = [ADerived:: (ADerived([1,i],null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),(1.,2.+i),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),Contained(i+0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(null(),null(),null(),[Contained(0.4d0,(i+.5d0,.6d0))],null(),null(),null(),null()), i=1,1)]

end program acetdt35d

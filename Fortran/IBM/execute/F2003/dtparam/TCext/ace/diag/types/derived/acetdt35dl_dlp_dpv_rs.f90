! GM DTP extension using:
! ftcx_dtp -qnok -ql -qdefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/ace/diag/types/derived/acetdt35d.f

!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : acetdt35dl_dlp_dpv_rs
!*
!*  PROGRAMMER                 : Glen Mateer (derived from acetdt35d
!*                               by David Forster)
!*  DATE                       : 2007-11-30 (original: 2006-11-13)
!*  ORIGIN                     : Compiler Development,
!*                               IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf2003)
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
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt35dl_dlp_dpv_rsmod

  implicit none

  type Contained(l1)    ! (20)
      integer, len :: l1
     double precision :: dp
     double complex   :: dz
  end type Contained

  type ADerived(k1,l2)    ! (4,1)
     integer, kind                   :: k1
     integer, len                    :: l2
     integer(k1), allocatable        :: iafield (:)
     complex(k1), allocatable        :: zafield
     type(Contained(:)), allocatable :: dafield
     type(Contained(:)), allocatable :: daArray(:)
     real(k1), pointer               :: rpfield
     character(l2), pointer          :: cpfield(:)
     type(Contained(:)), pointer     :: dpfield
     type(Contained(:)), pointer     :: dpArray(:)
  end type ADerived

end module acetdt35dl_dlp_dpv_rsmod


program acetdt35dl_dlp_dpv_rs

  use acetdt35dl_dlp_dpv_rsmod
  implicit none

  integer, parameter :: iParmArray(2) = [1,2]
  complex, parameter :: zParm = (1.234d-2,2.134d2)
  type (Contained(20)), parameter :: dParm = Contained(20)(5.6789d1,zParm**2)
  type (Contained(20)), parameter :: dParmArray(2) = [Contained:: Contained(20)(5.6789d1,zParm**2),Contained(20)(5.6789d1,zParm**2)]

  real, target       :: rt1 = 1.2
  character, target  :: ct(2) = ['a','b']
  type (Contained(20)), target :: dt = Contained(20)(1.1d1,(2.2d2,3.3d3))
  type (Contained(20)), target :: dtArray(2) = [Contained:: Contained(20)(4.4d4,(5.5d5,6.6d6)), Contained(20)(7.7d7,(8.8d8,9.9d9))]

  integer :: i

  ! allocatable with initialisation:
  type (Contained(:)), allocatable  :: cArr(:) = [Contained(20)(2d0,(3d0,4d0))]

  ! types with allocatable/pointer components:
  ! Verify that an all-null constructor is acceptable:
  type (ADerived(4,1)) :: adArray0(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),null(),null(),null())]

  ! Now test incorrect usage:
  type (ADerived(4,1)) :: adArray1(1) = [ADerived:: ADerived(4,1)(iParmArray,null(),null(),null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArray2(1) = [ADerived:: ADerived(4,1)(null(),zParm,null(),null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArray3(1) = [ADerived:: ADerived(4,1)(null(),null(),dParm,null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArray4(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),dParmArray,null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArray5(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),rt1,null(),null(),null())]
  type (ADerived(4,1)) :: adArray6(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),ct,null(),null())]
  type (ADerived(4,1)) :: adArray7(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),null(),dt,null())]
  type (ADerived(4,1)) :: adArray8(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),null(),null(),dtArray)]

  type (ADerived(4,1)) :: adArrayc1(1) = [ADerived:: ADerived(4,1)([1,2],null(),null(),null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArrayc2(1) = [ADerived:: ADerived(4,1)(null(),(1.,2.),null(),null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArrayc3(1) = [ADerived:: ADerived(4,1)(null(),null(),Contained(20)(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null())]
  type (ADerived(4,1)) :: adArrayc4(1) = [ADerived:: ADerived(4,1)(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null())]

  ! And repeat with implied-do:
  type (ADerived(4,1)) :: adArrayI1(1) = [ADerived:: (ADerived(4,1)(iParmArray,null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI2(1) = [ADerived:: (ADerived(4,1)(null(),zParm,null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI3(1) = [ADerived:: (ADerived(4,1)(null(),null(),dParm,null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI4(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),dParmArray,null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI5(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),rt1,null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI6(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),ct,null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI7(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),null(),dt,null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayI8(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),null(),null(),dtArray), i=1,1)]

  type (ADerived(4,1)) :: adArrayIc1(1) = [ADerived:: (ADerived(4,1)([1,2],null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayIc2(1) = [ADerived:: (ADerived(4,1)(null(),(1.,2.),null(),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayIc3(1) = [ADerived:: (ADerived(4,1)(null(),null(),Contained(20)(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null()), i=1,1)]
  type (ADerived(4,1)) :: adArrayIc4(1) = [ADerived:: (ADerived(4,1)(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null()), i=1,1)]

  ! Verify that the AC's are acceptable in the right context:

  adArray0 = [ADerived:: ADerived(4,1)(iParmArray,null(),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),zParm,null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),dParm,null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),dParmArray,null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),rt1,null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),ct,null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),null(),dt,null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),null(),null(),null(),null(),dtArray)]

  adArray0 = [ADerived:: ADerived(4,1)([1,2],null(),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),(1.,2.),null(),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),Contained(20)(0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null())]
  adArray0 = [ADerived:: ADerived(4,1)(null(),null(),null(),[Contained(0.4d0,(.5d0,.6d0))],null(),null(),null(),null())]

  adArray0 = [ADerived:: (ADerived(4,1)(iParmArray,null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),zParm,null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),dParm,null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),dParmArray,null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),rt1,null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),ct,null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),null(),dt,null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),null(),null(),null(),null(),dtArray), i=1,1)]

  adArray0 = [ADerived:: (ADerived(4,1)([1,i],null(),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),(1.,2.+i),null(),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),Contained(20)(i+0.1d0,(.2d0,.3d0)),null(),null(),null(),null(),null()), i=1,1)]
  adArray0 = [ADerived:: (ADerived(4,1)(null(),null(),null(),[Contained(0.4d0,(i+.5d0,.6d0))],null(),null(),null(),null()), i=1,1)]

end program acetdt35dl_dlp_dpv_rs

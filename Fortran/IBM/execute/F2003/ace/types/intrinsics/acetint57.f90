!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint57
!*
!*  DATE                       : 2006-11-23
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : default initialization of derived type _component_ (intrinsics)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic, component, default initialisation, default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Create a derived type with intrinsic array compoments; initialise these
!*  with a variety of AC's.
!*
!*  Changes:
!*  2007-02-12 dforster  constants modified to avoid inevitable roundoff error
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acetint57mod

  implicit none

  character(9), parameter :: charConsts(2) = [character(9):: 'prolixity', 'obviate']
  real(8), parameter :: realConsts(4) = [real(8) :: 9.67646821932d+08, -4.07229178932d+05, 11.97143634432d0, 3.141592653589793d+012]
  complex(16), parameter :: zConsts(2) = [complex(16) :: (9.81521024907605128983541q11, -5.79670837908828653852503q9), &
       & (-7.8870270068097745637302q+034, -1.5707963267948966192313216916398q+026)]
  integer :: i

  type derivedExplicit
     ! random values:
     integer (2)      :: iarr(2)  = [integer(2)::       1955, 6444]
     real(8)          :: rarr(2)  = [real(8)::          9.81521024907605128983541d11, -5.79670837908828653852503d9]
     complex(4)       :: zarr(2)  = [complex(4)::       (9.67646821932e+08, -4.07229178932e+05), (11.97143634432, 3.141592653589793e+012)]
     logical(1)       :: larr(6)  = [logical(1)::       .true., .false., .true., .false., .true., .true.]
     double precision :: dparr(2) = [double precision:: -7.887027000000001d+084, -1.16395046d+076]
     double complex   :: dzarr(2) = [double complex::   (-6.29742670042620296d+024, -1.0387858299999999d+027), &
          & (-5.39523550003193546d+057, -8.69775020011333044d+094)]
     character(9)     :: carr(2)  = [character(9)::     'prolixity', 'obviation']
  end type derivedExplicit

  type derivedImplied ! use implied-do's, change the order:
     character(9)     :: carr(2)  = [character(9)::     (charConsts(i), i=2,1,-1)]
     complex(4)       :: zarr(2)  = [complex(4)::       ((sin(realConsts(2*i+1)), cos(realConsts(2*i+2))), i=0,1)]
     double complex   :: dzarr(2) = [double complex::   (zConsts(i), i=1,2)]
     double precision :: dparr(2) = [double precision:: (dsqrt(i*(-7.887027000000001d+084)), i=-1,-2,-1)]
     integer (4)      :: iarr(2)  = [integer(4)::       (1955+ 6444*i, i=1,2)]
     logical(1)       :: larr(5)  = [logical(1)::       (i<=3, i=1,5)]
     real(8)          :: rarr(2)  = [real(8)::          (real(zConsts(i)), aimag(zConsts(i)), i=2,2)]
  end type derivedImplied

end module acetint57mod


program acetint57

  use acetint57mod
  implicit none
  type (derivedExplicit) :: dtex
  type (derivedExplicit), allocatable :: dtexa

  type (derivedImplied) :: dtimp
  type (derivedImplied), allocatable :: dtimpa

  print *, dtex
  allocate(dtexa)
  print *, dtexa

  print *, dtimp
  allocate(dtimpa)
  print *, dtimpa

end program acetint57

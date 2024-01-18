!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint35
!*
!*  DATE                       : 2006-11-14
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : intrinsic AC's in static initialisation
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : static, initialisation, initialization, AC
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that intrinsic arrays can be correctly initialised using an AC.
!*  - declare each type of intrinsic array, initialising it with an AC using
!*    the correct AC
!*  - print the values to verify correct initialisation
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint35

  implicit none
  integer          :: i, j

  ! Simple expressions:
  integer (4)      :: iarr(3) = [integer(4):: -1,0,1]
  real    (4)      :: rarr(3) = [real(4)::    -1e-38, 3.1415926536, 1e+38]
  complex (4)      :: zarr(3) = [complex(4):: (-1e-38,3.1415926536), (3.1415926536, 1e+38), (1e+38,-1e-38)]
  logical (4)      :: larr(3) = [logical(4):: .false., .true., size(iarr) == size(rarr)]
  double complex   :: dzar(3) = [double complex:: (3.1415926535897932384626433832795d0,2.7182818284590452353602874713527d0), &
                                                  (-2.7182818284590452353602874713527d0,-1.4142135623730950488016887242097d0), &
                                                  (1.4142135623730950488016887242097d0,-3.1415926535897932384626433832795d0)]
  double precision :: dpar(3) = [double precision:: -1d-300, sqrt(2d0), 1d+300]
  character(4)     :: char(3) = [character(4):: 'abcd', 'efgh', 'ijkl']

  ! implied-dos:
  integer (4)      :: iari(3) = [integer(4):: (i, i=-1,1)]
  real    (4)      :: rari(3) = [real(4)::    (10.0 ** i, i=-38,38,38)]
  complex (4)      :: zari(3) = [complex(4):: ((real(i) ** (i/10.),(i/10.)**real(i)), i=-38,38,38)]

  ! embedded AC's, some intrinsics:
  logical (4)      :: lari(3) = [logical(4):: (([.2,3.,.4]<[(1.0,j=1,3)]), i=1,1)]
  double complex   :: dzai1(3) = [double complex:: ([((real(j),real(i)), j=1,3)] ** 2, i=1,1)]
  double complex   :: dzai2(3) = [double complex:: ([(real(1),real(i)), (real(2),real(i)), (real(3),real(i))] ** 2, i=1,1)]
  double complex   :: dzai3(3) = [double complex:: [(1.0,1.0), (2.0,1.0), (3.0,1.0)] ** 2]
  double precision :: dpai(3) = [double precision:: (sin(3.1415926535897932384626433832795d0/i), i=1,3)]
  character(4)     :: chai(3) = [character(4):: (merge('mnop', 'qrst', i==2), i=1,3)]

  ! Now print them all:
  print *, iarr
  print *, rarr
  print *, zarr
  print *, larr
  print *, dzar
  print *, dpar
  print *, char
  print *, iari
  print *, rari
  print *, zari
  print *, lari
  print *, dzai1
  print *, dzai2
  print *, dzai3
  print *, dpai
  print *, chai

end program acetint35

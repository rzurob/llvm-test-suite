!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with named constant as
!*                               argument to C with %ref and option -qnullterm
!*
!* ===================================================================

    program mxminScalarArgBindC2

    external sindex
    integer sindex
    character*8, parameter :: x = 'abcd  gf'

    character*8, parameter :: y = 'dnkjsmoo'

    if (sindex(%ref(max(x, y)), 'K', %val(8)) /= 0) error stop 1_4

    end program mxminScalarArgBindC2


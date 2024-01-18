!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BElemental02
!*
!*  DATE                       : 2010-12-06
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : block in elemental procedure
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  block in elemental procedure - assign result to temporary before returning value
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BElemental02
    implicit none
    character(2) :: var(3)
    var = cfun([97,41,126],[49,52,120])
    print *, var, cfun([97,98,99],111)
    if (any(var /= ['a1',')4','~x'])) stop 2
    if (any(cfun([97,98,99],111) /= ['ao','bo','co'])) stop 3

  contains

    elemental character(2) function cfun(a, b)
      integer, intent(in) :: a, b
      character(2) :: tmp
      block
        tmp = achar(merge(a,128-a,a>=0)) // achar(merge(b,128-b,b>=0))
      end block
      cfun = tmp
    end function cfun

end program BElemental02

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal024.f 
! %VERIFY: ffinal024.out:ffinal024.vf
! %STDIN:
! %STDOUT: ffinal024.out 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal024.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : final subroutines 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DESCRIPTION                : testing final subroutines: import 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    integer :: int
    contains
       final :: finalizeBase
    end type
    contains
    subroutine finalizeBase (arg1)
       type (base), intent (in) :: arg1 
       print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
    contains
       final :: finalizeChild
    end type

    type(base)  :: dt1  

    interface interf
        subroutine fBase(arg1)
        import base
        type(base), intent(inout) :: arg1
        end subroutine
    end interface 

    contains

    subroutine finalizeChild (arg1)
        type (child), intent (in) :: arg1 
        print *, 'finalizeChild'
    end subroutine
 
end module

module m2
use m1
   type dt
      type(base) ::  dt_b 
      type(child) :: dt_c 
   end type
end module

use m2

    call interf(dt1)

end

subroutine fBase (arg1)
use m, only : base
use m2, only : dt
   type(base), intent(inout) :: arg1
   type(dt)  :: t1  
end subroutine 


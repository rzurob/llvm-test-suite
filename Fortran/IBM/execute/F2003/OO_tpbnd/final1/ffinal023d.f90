!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal023d.f 
! %VERIFY: ffinal023d.out:ffinal023d.vf
! %STDIN:
! %STDOUT: ffinal023d.out 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal023d.f
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

    interface print
        subroutine fBase(arg1)
        import base
        integer, intent(in) :: arg1
        end subroutine
  
        subroutine fChild(arg1, arg2)
        import child
        integer, intent(in) :: arg1, arg2
        end subroutine
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child), intent (in) :: arg1(:,:) 
        print *, 'finalizeChild'
    end subroutine
 
end module

use m1

    call print(10)
    call print(10, 10)

end

subroutine fBase (arg1)
use m, only : base 
   integer, intent(in) :: arg1
   type(base), allocatable  :: dt1  
   allocate(dt1)
   deallocate(dt1)
end subroutine 

subroutine fChild (arg1, arg2)
use m1, only : child
    integer, intent(in) :: arg1, arg2
    type(child), allocatable  :: dt1(:,:) 
    allocate(dt1(2,2))
    deallocate(dt1)
end subroutine 


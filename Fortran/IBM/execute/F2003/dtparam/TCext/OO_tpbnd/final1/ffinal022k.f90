!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal022k.f
!*  TEST CASE NAME             : type-bound procedure ffinal022k
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal022 by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: 
!*                               finalizations in  print statement
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
    integer(kbase_1) :: int
    contains
       final :: finalizeBase
    end type
    contains
    subroutine finalizeBase (arg1)
       type (base(4)), intent (in) :: arg1  ! tcx: (4)
       print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child
    contains
       final :: finalizeChild
    end type

    interface interf 
        function fBase()
        import base
           type(base(4)) :: fBase ! tcx: (4)
        end function
  
        function fChild(arg1)
        import child
           type(child(4)) :: fChild ! tcx: (4)
           integer, intent(in) :: arg1 
        end function
    end interface

    contains

    subroutine finalizeChild (arg1)
        type (child(4)), intent (in) :: arg1  ! tcx: (4)
        print *, 'finalizeChild'
    end subroutine
 
end module

use m1

    print *, interf()
    print *, interf(10)

end

function fBase ()
use m, only : base
   type(base(4)) :: fBase  ! tcx: (4)
   fBase%int = 10
end function

function fChild (arg1)
use m1, only : child
    type (child(4))  :: fChild  ! tcx: (4)
    integer, intent(in) :: arg1 
    fChild%int = arg1 
end function



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 3 changes

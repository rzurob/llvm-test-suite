!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal017dk.f
!*  TEST CASE NAME             : type-bound procedure ffinal017dk
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal017d by Catherine Sun)
!*  DATE                       : 2007-11-26 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines are not inherited  
!*                               through type extension.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
       integer(kbase_1) :: x
    contains
       final :: finalizeBase
    end type
    
    type dt (kdt_1) ! kdt_1=4
       integer, kind :: kdt_1
       type(base(kdt_1)), pointer :: dt_p => null() ! tcx: (kdt_1)
    contains
       final :: finalize 
    end type

    contains
    subroutine finalizeBase (b1)
        type (base(4)), intent(inout) :: b1 ! tcx: (4)
        print *, 'finalizeBase'
    end subroutine

    subroutine finalize (d1)
        type (dt(4)), intent(inout) :: d1 ! tcx: (4)
        if(associated(d1%dt_p))    deallocate(d1%dt_p) 
    end subroutine
end module

   use m 

   call sub()

end

subroutine sub()
    use m
    type(dt(4)) :: dt_c  ! tcx: (4)
    allocate(dt_c%dt_p)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: dt - added parameters (kdt_1) to invoke with (4) / declare with (4) - 2 changes

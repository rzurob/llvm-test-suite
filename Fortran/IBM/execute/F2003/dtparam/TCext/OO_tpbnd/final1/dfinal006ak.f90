!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal006ak.f
!*  TEST CASE NAME             : type-bound procedure dfinal006ak
!*
!*  PROGRAMMER                 : David Forster (derived from dfinal006a by Catherine Sun)
!*  DATE                       : 2007-11-12 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines:
!*                               dup "final" keyword
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
        contains
        procedure, nopass :: final
        final  :: final
    end type
    
    contains
    subroutine final (b1)
       type(base(4)), intent(in) :: b1  ! tcx: (4)
       print *, 'finalizeBase'
    end subroutine
end module

   use m
   type(base(4)) :: dt ! tcx: (4)

   call dt%final(dt)

   print *, 'second test'

   call sub (dt)

   print *, 'end'
end

subroutine sub(arg1)
   use m
   type(base(4)), intent(out) :: arg1  ! tcx: (4)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes

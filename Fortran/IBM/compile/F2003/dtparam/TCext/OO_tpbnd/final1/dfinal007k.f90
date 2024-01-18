!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal007k.f
!*  TEST CASE NAME             : type-bound procedure dfinal007k
!*
!*  PROGRAMMER                 : David Forster (derived from dfinal007 by Catherine Sun)
!*  DATE                       : 2007-11-12 (original: )
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters final subroutines 
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : testing final subroutines: A final  
!*                               -subroutine-name shall not be one
!*                               previously specified as a binding name
!*                               for a type-bound procedure with pass
!*                               attribute. 
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: x
        contains
        procedure  :: finalizeBase !* expect an error message here
        final  :: finalizeBase 
    end type
    
    contains
    subroutine finalizeBase (b1)
       type(base(4)) :: b1  ! tcx: (4)
       print *, 'finalizeBase'
    end subroutine
end module
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes

!***********************************************************************
!* =====================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : abstracti017kl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti017
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-25 (original: 02/20/2006)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: the derived-type-spec shall not
!*                               specify an ABSTRACT type (C401) SelectType
!*                               Construct: type-guard-stmt CLASS IS
!*                               specifies type-spec is Abstract type
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: b1 (kb1_1) ! kb1_1=4
      integer, kind :: kb1_1
      integer(kb1_1) :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2 (lb1_1) ! lb1_1=1
      integer, len :: lb1_1
   contains
      procedure, nopass :: print
   end type

   abstract interface
      subroutine printif()
      end subroutine
   end interface
contains
   subroutine print()
      print *,'b2'
   end subroutine
end module

program abstracti017kl
   use m
   class(b1(4)), allocatable :: b11 ! tcx: (4)
   allocate(b2(4,1) :: b11) ! tcx: (4,1)

   select type ( b => b11 )
      class is ( b1(4) ) ! tcx: (4)
         call b%print()
   end select

end program abstracti017kl


! Extensions to introduce derived type parameters:
! type: b1 - added parameters (kb1_1) to invoke with (4) / declare with (4) - 2 changes
! type: b2 - added parameters (lb1_1) to invoke with (4,1) / declare with (4,1) - 1 changes

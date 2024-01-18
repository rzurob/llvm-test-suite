! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C574_002kl
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: C574
!*                                        A namelist group object shall be able to be an assumed shape array
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base1 (kb1) ! kb1=4
      integer, kind :: kb1
      real(kb1), allocatable :: i
      real(kb1), pointer     :: j
   end type
end module

module m
   type base (kb) ! kb=4
      integer, kind :: kb
      real(kb), allocatable :: i
      real(kb), pointer     :: j
   end type

   interface
      subroutine bar1(b)
         import base
         class(base(4)) :: b(3:) ! tcx: (4)
      end subroutine
   end interface

contains
   subroutine foo(a)
      integer(4) :: a(2:)
      namelist /nml/ a
   end subroutine

   subroutine foo1(b)
      class(base(4)) :: b(-1:) ! tcx: (4)
      namelist /nml/ b
    end subroutine

end module

program C574_002kl
end program

subroutine bar1(b)
   use m1
   class(base1(4)) :: b(:) ! tcx: (4)
   namelist /nml/ b
end subroutine


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kb1) to invoke with (4) / declare with (4) - 1 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes

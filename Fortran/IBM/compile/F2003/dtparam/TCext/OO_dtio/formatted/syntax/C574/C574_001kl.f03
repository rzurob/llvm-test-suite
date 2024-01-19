! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: C574
!*                                        A namelist group object shall not be an assumed size array
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
         class(base(4)) :: b(1,*) ! tcx: (4)
      end subroutine
   end interface

contains
   subroutine foo(a)
      integer(4) :: a(2:3,*)
      namelist /nml/ a

      print *, a(2,1), a(3,1)
   end subroutine

   subroutine foo1(b)
      class(base(4)) :: b(1,*) ! tcx: (4)
      namelist /nml/ b
      print *, b(1,1)%i, b(1,1)%j
   end subroutine

end module

program C574_001kl
end program

subroutine bar1(b)
   use m1
   class(base1(4)) :: b(1,*) ! tcx: (4)
   namelist /nml/ b
   print *, b(1,1)%i, b(1,1)%j
end subroutine


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kb1) to invoke with (4) / declare with (4) - 1 changes
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes

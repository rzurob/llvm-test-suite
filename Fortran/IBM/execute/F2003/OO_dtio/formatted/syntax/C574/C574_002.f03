! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
   type base1
      real(4), allocatable :: i
      real(4), pointer     :: j
   end type
end module

module m
   type base
      real(4), allocatable :: i
      real(4), pointer     :: j
   end type

   interface
      subroutine bar1(b)
         import base
         class(base) :: b(3:)
      end subroutine
   end interface

contains
   subroutine foo(a)
      integer(4) :: a(2:)
      namelist /nml/ a
   end subroutine

   subroutine foo1(b)
      class(base) :: b(-1:)
      namelist /nml/ b
    end subroutine

end module

program C574_002
end program

subroutine bar1(b)
   use m1
   class(base1) :: b(:)
   namelist /nml/ b
end subroutine

! GM DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/abstracti/unit_tests/unit/abifce04.f

!***********************************************************************
!* =====================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : abifce04kl
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abifce04
!*                               by James Ren)
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf90/95)
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The use of procedure pointers with the
!*  interfaces declared in an abstract interface
!*  They can point to any procedures as long as
!*  they have the same interface
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

abstract interface
   function func (x)
      integer, intent (in) :: x
      logical :: func
   end function

   subroutine print_out(n, result)
      integer, intent (in) :: n
      logical, intent (in) :: result
   end subroutine
end interface

logical result
integer n

!-- a derived type with a procedure pointer component
type struct_type(k1,l1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: l1
   procedure (func), pointer, nopass :: component =>null()
end type struct_type
! Procedures with the same interface as defined in an abstract interface
procedure (func) prime
procedure (print_out) print_result
! procedure pointers with explicit interface and initialized to null().
procedure (func), pointer :: p => null()
procedure (print_out), pointer :: r => null()

!-- ... and a variable of that type.
type(struct_type(4,20)) :: struct

! prime and print_result are procedure pointers with a
! compatible interface.
p => prime
r => print_result
n = 10
result = p(10)
call r(n, result)

! likewise for a structure component.
struct % component => prime
n = 11
result = struct % component(n)
call r(n, result)
end

logical function prime(n)
   integer i, j
   i = 2
   j = sqrt(real(n))
   do  10, while ((i .le. j) .and. (mod(n, i) .ne. 0))
      i = i + 1
10 continue
   if( i .gt. j) then
      prime = .true.
   else
      prime = .false.
   end if
end  function

subroutine print_result(n, result)
   integer, intent (in) :: n
   logical, intent (in) :: result
   if ( result ) then
      print *, n, ' is a prime number'
   else
      print *, n, ' is not a prime number'
   end if
end subroutine

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 20, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : BIND(C) procedures defined in Fortran,
!*                               call in Fortran
!*                               dummy argument is scalar assumed type with
!*                                    - the optional attribute
!*                                    - the target attribute
!*                                    - the volatile attribute
!*                               actual argument is of Fortran intrinsic type
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType05f
implicit none

interface
   logical(c_bool) function fnc_optional(a) bind(c)
      use, intrinsic :: iso_c_binding
      type(*), optional :: a

   end function fnc_optional

   subroutine sub_optional(a) bind(c)
      use, intrinsic :: iso_c_binding
      type(*), optional :: a

   end subroutine sub_optional

   function fnc_target(a) result(res) bind(c)
      use, intrinsic :: iso_c_binding
      type(*), target :: a
      logical(c_bool) :: res

   end function fnc_target

   subroutine sub_target(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), target :: a

   end subroutine sub_target

   logical(c_bool) function fnc_volatile(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), volatile :: a

   end function fnc_volatile

   subroutine sub_volatile(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*), volatile :: a

   end subroutine sub_volatile
end interface

integer       :: i
integer*1     :: i1
integer*2     :: i2
integer*4     :: i4
integer*8     :: i8
integer(8)    :: i88
real          :: r
real*4        :: r4
real(4)       :: r44
real*8        :: r8
real*16       :: r16
complex       :: z
complex*8     :: z8
complex(4)    :: z88
complex*16    :: z32
complex(16)   :: z16
logical       :: l
logical*1     :: l1
logical*2     :: l2
logical*4     :: l4
logical*8     :: l8
character*10  :: c1
character(10) :: c2

call sub_optional(i)
call sub_optional(i1)
call sub_optional(i2)
call sub_optional(i4)
call sub_optional(i8)
call sub_optional(i88)
call sub_optional(r)
call sub_optional(r4)
call sub_optional(r44)
call sub_optional(r8)
call sub_optional(r16)
call sub_optional(z)
call sub_optional(z8)
call sub_optional(z88)
call sub_optional(z32)
call sub_optional(z16)
call sub_optional(l)
call sub_optional(l1)
call sub_optional(l2)
call sub_optional(l4)
call sub_optional(l8)
call sub_optional(c1)
call sub_optional(c2)

l = fnc_optional(i)
l = fnc_optional(i1)
l = fnc_optional(i2)
l = fnc_optional(i4)
l = fnc_optional(i8)
l = fnc_optional(i88)
l = fnc_optional(r)
l = fnc_optional(r4)
l = fnc_optional(r44)
l = fnc_optional(r8)
l = fnc_optional(r16)
l = fnc_optional(z)
l = fnc_optional(z8)
l = fnc_optional(z88)
l = fnc_optional(z32)
l = fnc_optional(z16)
l = fnc_optional(l)
l = fnc_optional(l1)
l = fnc_optional(l2)
l = fnc_optional(l4)
l = fnc_optional(l8)
l = fnc_optional(c1)
l = fnc_optional(c2)

call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()
call sub_optional()

l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()
l = fnc_optional()

call sub_target(i)
call sub_target(i1)
call sub_target(i2)
call sub_target(i4)
call sub_target(i8)
call sub_target(i88)
call sub_target(r)
call sub_target(r4)
call sub_target(r44)
call sub_target(r8)
call sub_target(r16)
call sub_target(z)
call sub_target(z8)
call sub_target(z88)
call sub_target(z32)
call sub_target(z16)
call sub_target(l)
call sub_target(l1)
call sub_target(l2)
call sub_target(l4)
call sub_target(l8)
call sub_target(c1)
call sub_target(c2)

l = fnc_target(i)
l = fnc_target(i1)
l = fnc_target(i2)
l = fnc_target(i4)
l = fnc_target(i8)
l = fnc_target(i88)
l = fnc_target(r)
l = fnc_target(r4)
l = fnc_target(r44)
l = fnc_target(r8)
l = fnc_target(r16)
l = fnc_target(z)
l = fnc_target(z8)
l = fnc_target(z88)
l = fnc_target(z32)
l = fnc_target(z16)
l = fnc_target(l)
l = fnc_target(l1)
l = fnc_target(l2)
l = fnc_target(l4)
l = fnc_target(l8)
l = fnc_target(c1)
l = fnc_target(c2)

call sub_volatile(i)
call sub_volatile(i1)
call sub_volatile(i2)
call sub_volatile(i4)
call sub_volatile(i8)
call sub_volatile(i88)
call sub_volatile(r)
call sub_volatile(r4)
call sub_volatile(r44)
call sub_volatile(r8)
call sub_volatile(r16)
call sub_volatile(z)
call sub_volatile(z8)
call sub_volatile(z88)
call sub_volatile(z32)
call sub_volatile(z16)
call sub_volatile(l)
call sub_volatile(l1)
call sub_volatile(l2)
call sub_volatile(l4)
call sub_volatile(l8)
call sub_volatile(c1)
call sub_volatile(c2)

l = fnc_volatile(i)
l = fnc_volatile(i1)
l = fnc_volatile(i2)
l = fnc_volatile(i4)
l = fnc_volatile(i8)
l = fnc_volatile(i88)
l = fnc_volatile(r)
l = fnc_volatile(r4)
l = fnc_volatile(r44)
l = fnc_volatile(r8)
l = fnc_volatile(r16)
l = fnc_volatile(z)
l = fnc_volatile(z8)
l = fnc_volatile(z88)
l = fnc_volatile(z32)
l = fnc_volatile(z16)
l = fnc_volatile(l)
l = fnc_volatile(l1)
l = fnc_volatile(l2)
l = fnc_volatile(l4)
l = fnc_volatile(l8)
l = fnc_volatile(c1)
l = fnc_volatile(c2)

end program AssumedType05f

logical(c_bool) function fnc_optional(a)
   use, intrinsic :: iso_c_binding
   type(*), optional :: a

   fnc_optional = .true.

end function fnc_optional

subroutine sub_optional(a)
   use, intrinsic :: iso_c_binding
   type(*), optional :: a

end subroutine sub_optional

function fnc_target(a) result(res)
   use, intrinsic :: iso_c_binding
   type(*), target :: a
   logical(c_bool) :: res

   res = .true.

end function fnc_target

subroutine sub_target(a)
   use, intrinsic :: iso_c_binding
   type(*), target :: a

   print*, shape(a)
end subroutine sub_target

logical(c_bool) function fnc_volatile(a)
   use, intrinsic :: iso_c_binding
   type(*), volatile :: a

   fnc_volatile = .true.

end function fnc_volatile

subroutine sub_volatile(a)
   use, intrinsic :: iso_c_binding
   type(*), volatile :: a

end subroutine sub_volatile

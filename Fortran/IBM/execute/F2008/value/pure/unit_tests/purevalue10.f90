!******************************************************************************
!*  ===========================================================================
!*  XL Fortran Test Case                                  IBM INTERNAL USE ONLY
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue10.f
!*  TEST CASE TITLE : F2008: VALUE attr allowed for dummy args of PURE proc
!*  PROGRAMMER      : Gaby Baghdadi
!*  DATE            : 2010-12-01
!*  ORIGIN          : XL Fortran Compiler Development, IBM Torolab
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - C main calls Fortran pure function, passing a nested struct as arg, dummy
!*    arg derived type defined with value attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT) :: a
      integer(C_SHORT) :: b
   end type

   type, bind(c) :: dt1
      integer(C_INT) :: a
      integer(C_SHORT) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT) :: a
      integer(C_SHORT) :: b
      type(dt1) :: d1
   end type
end module

integer(C_INT) pure function foo(dt) bind(c)
   use ISO_C_BINDING
   use m

   type(dt2), value :: dt
   integer :: tmp
   tmp = 1

   if ( dt%a /= 1 ) tmp = tmp/0
   if ( dt%b /= 2 ) tmp = tmp/0
   if ( dt%d1%a /= 3 ) tmp = tmp/0
   if ( dt%d1%b /= 4 ) tmp = tmp/0
   if ( dt%d1%d0%a /= 5 ) tmp = tmp/0
   if ( dt%d1%d0%b /= 6 ) tmp = tmp/0

   dt%a = dt%a * 2
   dt%b = dt%b * 3
   dt%d1%a = dt%d1%a * 4
   dt%d1%b = dt%d1%b * 5
   dt%d1%d0%a = dt%d1%d0%a * 6
   dt%d1%d0%b = dt%d1%d0%b * 7

   foo = 0
end function foo


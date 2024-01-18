!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type array allocatable components
!*                                 - actual arg: (non-)polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type inner
      integer, allocatable :: i(:)
   end type

   type base
      type(inner), allocatable :: in(:)
      character(1), allocatable :: c(:)
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent003
   use m

   type(base) :: b1
   class(base), target, allocatable :: b2
   class(base), pointer :: b3

   integer :: j(3) = (/ 1,2,3 /)
   character(1) :: c(3) = (/ 'a','b','c' /)

   b1 = base( (/ inner( j ), inner( (/ j, 4 /) ), inner( (/ j,4,5 /) ) /), c )
   call foo ( b1 )

   do k=lbound(b1%in,1), ubound(b1%in,1)
      print *, b1%in(k)%i
   end do
   print *, b1%c

   allocate ( b2 )
   allocate ( b2%in(4:5), source = (/ inner( j(3:1:-1) ), inner( (/ j(3:1:-1), 0 /) ) /) )
   allocate ( b2%c(10:12), source = (/ 'i','b','m' /) )

   call foo ( b2 )

   do k=lbound(b2%in,1), ubound(b2%in,1)
      print *, b2%in(k)%i
   end do
   print *, b2%c

   allocate ( b3, source = b1 )

   call foo ( b3 )

   do k=lbound(b3%in,1), ubound(b3%in,1)
      print *, b3%in(k)%i
   end do
   print *, b3%c

end program


subroutine foo( dtv )
   use m, only: base, inner
   type(base), value :: dtv

   print *, "INSIDE foo"
   do j=lbound(dtv%in,1), ubound(dtv%in,1)
      print *, dtv%in(j)%i
      dtv%in(j)%i = -999
   end do

   print *, dtv%c
   dtv%c = 'x'

   do j=lbound(dtv%in,1), ubound(dtv%in,1)
      print *, dtv%in(j)%i
   end do

   print *, dtv%c
   
   print *, "END foo"

end subroutine

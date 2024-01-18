! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qnodeferredlp -qreuse=self -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent004.f
! opt variations: -qck -qnok -ql -qdefaultpv -qdeferredlp -qreuse=none

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
!*                                 - type: derived type polymorphic array allocatable components
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

   type inner(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i(:)
   end type

   type, extends(inner) :: cinner    ! (4)
      integer(k1), allocatable :: j(:)
   end type

   type base(k2,n1)    ! (4,1)
      integer, kind                 :: k2
      integer, len                  :: n1
      class(inner(k2)), allocatable :: in(:)
      character(:), allocatable     :: c(:)
   end type

   interface
      subroutine foo( dtv )
         import base
         type(base(4,1)), value :: dtv
      end subroutine
   end interface

end module

program valueScalarAllocatableComponent004
   use m

   type(base(4,1)) :: b1
   class(base(4,1)), target, allocatable :: b2
   class(base(4,1)), pointer :: b3

   integer :: j(3) = (/ 1,2,3 /)
   integer :: k(5) = (/ 1,2,3, 4, 5 /)
   character(1) :: c(3) = (/ 'a','b','c' /)

   b1 = base(4,1)( (/ cinner(4)( j, k ), cinner(4)( k,j ), cinner(4)( (/ j,4,5 /), k ) /), c )
   call foo ( b1 )

   do l=lbound(b1%in,1), ubound(b1%in,1)
      select type ( g => b1%in )
         type is ( cinner(4) )
            print *, g(l)%i, g(l)%j
      end select
   end do

   print *, b1%c

   allocate ( b2 )
   allocate ( b2%in(4:5), source = (/ cinner(4)( j(3:1:-1), k(5:1:-2) ), cinner(4)(  j(3:1:-1), k(4:2:-2)  ) /) )
   allocate ( b2%c(10:12), source = (/ 'i','b','m' /) )

   call foo ( b2 )

   do l=lbound(b2%in,1), ubound(b2%in,1)
      select type ( g => b2%in )
         type is ( cinner(4) )
            print *, g(l)%i, g(l)%j
      end select
   end do

   print *, b2%c

   b3 => b2

   call foo ( b3 )

   do l=lbound(b2%in,1), ubound(b2%in,1)
      select type ( g => b2%in )
         type is ( cinner(4) )
            print *, g(l)%i, g(l)%j
      end select
   end do

   print *, b2%c

   do l=lbound(b3%in,1), ubound(b3%in,1)
      select type ( g => b3%in )
         type is ( cinner(4) )
            print *, g(l)%i, g(l)%j
      end select
   end do

   print *, b3%c

end program


subroutine foo( dtv )
   use m, only: base, inner, cinner
   type(base(4,1)), value :: dtv

   print *, "INSIDE foo"
   do j=lbound(dtv%in,1), ubound(dtv%in,1)
      select type ( g => dtv%in )
         type is (inner(4))
            print *, g(j)%i
            g(j)%i = -999
         type is ( cinner(4) )
            print *, g(j)%i, g(j)%j
            g(j)%i = -999
            g(j)%j = -999
      end select
   end do

   print *, dtv%c
   dtv%c = 'x'

   do j=lbound(dtv%in,1), ubound(dtv%in,1)
      select type ( g => dtv%in )
         type is (inner(4))
            print *, g(j)%i
         type is ( cinner(4) )
            print *, g(j)%i, g(j)%j
      end select
   end do

   print *, dtv%c

   print *, "END foo"

end subroutine

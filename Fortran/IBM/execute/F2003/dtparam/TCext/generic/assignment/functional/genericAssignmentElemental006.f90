! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentElemental006.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self -qreuse=none

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
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: polymorphic elemental assignment for type component as well as type
!*                                           with child type containing overridding tb
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
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: iassgn
         generic :: assignment(=) => iassgn
   end type

   type, extends(inner) :: cinner    ! (4)
      integer(k1) :: j
      contains
         procedure, pass :: iassgn => ciassgn
   end type

   type base(k2)    ! (4)
      integer, kind                 :: k2
      class(inner(k2)), allocatable :: in(:)
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      elemental subroutine ciassgn ( a, b )
         class(cinner(4)), intent(out) :: a
         class(inner(4)), intent(in) :: b

         a%i = b%i + 1

         select type ( b )
            type is ( cinner(4) )
               a%j = b%j + 1
         end select

      end subroutine

      elemental subroutine iassgn ( a, b )
         class(inner(4)), intent(out) :: a
         class(inner(4)), intent(in) :: b

         a%i = b%i + 1

      end subroutine

      elemental subroutine bassgn ( a, b )
         class(base(4)), intent(out) :: a
         type(base(4)), intent(in) :: b

         if ( allocated(b%in) ) then
            if ( .not. allocated(a%in) ) allocate ( a%in(size(b%in)), source = b%in )
            a%in = b%in
         end if


      end subroutine

end module

program genericAssignmentElemental006
   use m

   type(base(4)) :: b1
   type(base(4)) :: b2(3)
   type(base(4)), allocatable :: b3(:)

   b1 = base(4)((/inner(4)(100)/))
   print *, b1%in%i

   b2 = base(4)((/ cinner(4)(200,300) /))

   select type ( g=> b2(1)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   select type ( g=> b2(2)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   select type ( g=> b2(3)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   allocate ( b3(5) )
   b3 = base(4)((/cinner(4)(300,400), cinner(4)(500,600)/))

   select type ( g=> b3(1)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(2)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(3)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(4)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(5)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   b2 = (/ base(4)((/cinner(4)(2000, 2001)/)), base(4)((/cinner(4)(2002, 2003), cinner(4)(2004, 2005)/)), base(4)( (/ inner(4)(2006),inner(4)(2007),inner(4)(2008) /) ) /)

   select type ( g=> b2(1)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   select type ( g=> b2(2)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

   select type ( g=> b2(3)%in )
      type is (inner(4))
         print *, g%i
   end select

   b3 = (/ b2, base(4)( (/inner(4)(2004), inner(4)(2005), inner(4)(2006)/) ), base(4)( (/cinner(4)(2007,207), cinner(4)(2008,208), cinner(4)(2009,209)/) ) /)

   select type ( g=> b3(1)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(2)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select
   select type ( g=> b3(3)%in )
      type is (inner(4))
         print *, g%i
   end select
   select type ( g=> b3(4)%in )
      type is (inner(4))
         print *, g%i
   end select
   select type ( g=> b3(5)%in )
      type is (cinner(4))
         print *, g%i, g%j
   end select

end program

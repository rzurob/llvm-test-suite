! GB DTP extension using:
! ftcx_dtp -qnodeferredlp -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent002.f
! opt variations: -qck -qdeferredlp -qreuse=none

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
!*                                 - type: intrinsic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: non-polymorphic with value attribute
!*                                    - also check bounds
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

   type base(k1,n1)    ! (4,1)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1), allocatable   :: i(:,:)
      character(n1), allocatable :: c(:)
      contains
         procedure, pass :: geti
         procedure, pass :: getc
   end type

   type, extends(base) :: child    ! (4,1)
      real(k1), allocatable :: r(:)
      contains
         procedure, pass :: getr
   end type

   class(base(4,1)), pointer :: b2

   contains

      integer function geti ( a )
         class(base(4,*)), intent(in) :: a
         allocatable :: geti
         dimension :: geti(:,:)
         allocate ( geti( size(a%i,1), size(a%i,2) ), source = a%i )
      end function

      character(1) function getc ( a )
         class(base(4,*)), intent(in) :: a
         allocatable :: getc(:)
         allocate ( getc(size(a%c)), source = a%c )
      end function

      real function getr ( a )
         class(child(4,*)), intent(in) :: a
         allocatable :: getr(:)
         allocate ( getr(size(a%r)), source = a%r )
      end function

      subroutine foo( dtv )
         type(base(4,1)), value :: dtv

         print *, lbound(dtv%i,1),ubound(dtv%i,1), lbound(dtv%i,2), ubound(dtv%i,2), ":", dtv%geti()
         print *, lbound(dtv%c,1),ubound(dtv%c,1),  ":", dtv%getc()
         dtv%i = -999
         dtv%c = 'x'
         print *, lbound(dtv%i,1),ubound(dtv%i,1), lbound(dtv%i,2), ubound(dtv%i,2),  ":", dtv%geti()
         print *, lbound(dtv%c,1),ubound(dtv%c,1),":", dtv%getc()

         deallocate ( dtv%i, dtv%c )

      end subroutine

      subroutine bar( dtv )
         type(child(4,1)), value :: dtv

         print *, lbound(dtv%i,1),ubound(dtv%i,1), lbound(dtv%i,2), ubound(dtv%i,2),":", dtv%geti()
         print *, lbound(dtv%c,1),ubound(dtv%c,1), ":",dtv%getc()
         print *, lbound(dtv%r,1),ubound(dtv%r,1),":", dtv%getr()
         dtv%i = -999
         dtv%c = 'xxxxx'
         dtv%r = -1.0
         print *, lbound(dtv%i,1),ubound(dtv%i,1), lbound(dtv%i,2), ubound(dtv%i,2),":", dtv%geti()
         print *, lbound(dtv%c,1),ubound(dtv%c,1),":", dtv%getc()
         print *, lbound(dtv%r,1),ubound(dtv%r,1), ":",dtv%getr()

         deallocate ( dtv%i, dtv%c, dtv%r )

      end subroutine

end module

program valueArrayAllocatableComponent002
   use m

   class(base(4,1)), allocatable  :: b1
   class(child(4,1)), pointer     :: c1

   allocate ( b1 )
   allocate (b1%i(-1:0, 0:1), source = reshape ( source = (/1,2,3,4/), shape = (/2,2/) ) )
   allocate (b1%c(10:12), source = (/ 'i', 'b', 'm'/) )

   call foo ( b1 )

   print *, lbound(b1%i,1),ubound(b1%i,1), lbound(b1%i,2), ubound(b1%i,2),":", b1%geti()
   print *, lbound(b1%c,1),ubound(b1%c,1),":", b1%getc()

   allocate ( c1 )
   allocate (c1%i(-100:-98, 98:100), source = reshape ( source = (/5,6,7,8,9,10,11,12,13/), shape = (/3,3/) ) )
   allocate (c1%c(10:12), source = (/ 'f','t','n' /) )
   allocate (c1%r(10000:10001), source = (/1.0, 2.0/) )

   call bar ( c1 )

   print *, lbound(c1%i,1),ubound(c1%i,1), lbound(c1%i,2), ubound(c1%i,2),":", c1%geti()
   print *, lbound(c1%c,1),ubound(c1%c,1),":", c1%getc()
   print *, lbound(c1%r,1),ubound(c1%r,1),":", c1%getr()

   allocate ( child(4,1) :: b2 )

   select type ( b2 )
      type is ( child(4,*) )
         allocate (b2%i(-1000:-998, 998:1000), source = reshape ( source = (/5,6,7,8,9,10,11,12,13/), shape = (/3,3/) ) )
         allocate (b2%c(12:10) )
         allocate (b2%r(10000:9999) )
   end select

   call foo ( b2 )

   select type ( b2 )
      type is ( child(4,*) )
         print *, lbound(b2%i,1),ubound(b2%i,1), lbound(b2%i,2), ubound(b2%i,2),":", b2%geti()
         print *, lbound(b2%c,1),ubound(b2%c,1),":", b2%getc()
         print *, lbound(b2%r,1),ubound(b2%r,1),":", b2%getr()
   end select

end program

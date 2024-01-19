! GB DTP extension using:
! ftcx_dtp -qdeferredlp -qreuse=base /tstdev/F2003/valueAttrwAllocCompnt/valueScalarAllocatableComponent002.f
! opt variations: -qck -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: intrinsic scalar allocatable components
!*                                 - actual arg: polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: polymorphic with value attribute
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

   type base(k1,n1)    ! (4,5)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1), allocatable   :: i
      character(n1), allocatable :: c
      contains

         procedure, pass :: geti
         procedure, pass :: getc

   end type

   type, extends(base) :: child    ! (4,5)
      real(k1), allocatable :: r
      contains

         procedure, pass :: getr

   end type

   class(child(4,:)), pointer :: c2

   interface
      subroutine foo( dtv )
         import base
         type(base(4,5)), value :: dtv
      end subroutine
   end interface

   interface
      subroutine bar( dtv )
         import child
         type(child(4,5)), value :: dtv
      end subroutine
   end interface

   contains

      integer function geti ( a )
         class(base(4,*)), intent(in) :: a
         geti = a%i
      end function

      character(5) function getc ( a )
         class(base(4,*)), intent(in) :: a
         getc = a%c
      end function

      real function getr ( a )
         class(child(4,*)), intent(in) :: a
         getr = a%r
      end function

end module

program valueScalarAllocatableComponent002
   use m

   type(base(4,5))  :: b1
   type(child(4,5)) :: c1
   class(base(4,:)), allocatable :: b2

   b1 = base(4,5)(1,'abcde')
   c1 = child(4,5)(2,'ABCDE', 10.0 )

   call foo( b1 )
   print *, b1%i, b1%c

   call bar( c1 )
   print *, c1%i, c1%c, c1%r

   allocate ( b2, source = base(4,5)(3,'fghij' ) )

   call foo ( b2 )
   print *, b2%i, b2%c

   allocate ( c2, source = child(4,5) ( 4, "FGHIJ", 20.0 ) )
   call bar ( c2 )

   print *, c2%i, c2%c, c2%r

   deallocate ( b2 )
   allocate ( b2, source = child(4,5) ( 5, "house", 30.0 ) )

   call foo( b2 )
   select type ( b2 )
      type is ( child(4,*) )
         print *, b2%i, b2%c, b2%r
   end select

end program


subroutine foo( dtv )
   use m, only: base
   type(base(4,5)), value :: dtv

   print *, dtv%geti(), dtv%getc()
   dtv%i = -999
   dtv%c = 'xxxxx'
   print *, dtv%i, dtv%c

end subroutine

subroutine bar( dtv )
   use m, only: base, child
   type(child(4,5)), value :: dtv

   print *, dtv%geti(), dtv%getc(), dtv%getr()
   dtv%i = -999
   dtv%c = 'xxxxx'
   dtv%r = -1.0
   print *, dtv%i, dtv%c, dtv%r

end subroutine


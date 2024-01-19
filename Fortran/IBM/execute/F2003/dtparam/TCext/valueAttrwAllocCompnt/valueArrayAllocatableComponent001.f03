! GB DTP extension using:
! ftcx_dtp -qdeferredlp -qreuse=self /tstdev/F2003/valueAttrwAllocCompnt/valueArrayAllocatableComponent001.f
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
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
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

   type base(k1,n1)    ! (4,1)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1), allocatable   :: i(:,:)
      character(n1), allocatable :: c(:)
      real(k1), allocatable      :: r(:)

      contains

         procedure, pass :: geti
         procedure, pass :: getc
         procedure, pass :: getr

   end type

   type(base(4,:)), pointer :: b3

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
         class(base(4,*)), intent(in) :: a
         allocatable :: getr(:)
         allocate ( getr(size(a%r)), source = a%r )
      end function

      subroutine foo( dtv )
         type(base(4,1)), VALUE :: dtv

         print *, dtv%geti(), dtv%getc(), dtv%getr()
         deallocate ( dtv%i, dtv%c, dtv%r )
         print *, allocated(dtv%i), allocated(dtv%c), allocated(dtv%r)

      end subroutine

end module

program valueArrayAllocatableComponent001
   use m

   type(base(4,1)) :: b1
   type(base(4,:)), allocatable :: b2

   b1 = base(4,1)( reshape ( source = (/1,2,3,4/), shape = (/2,2/) ), (/ 'i','b','m' /), (/ 1.0, 2.0, 3.0, 4.0, 5.0 /) )
   call foo ( b1 )
   print *, allocated(b1%i), allocated(b1%c), allocated(b1%r)
   print *, b1%i, b1%c, b1%r

   allocate ( b2, source = base(4,1)( reshape ( source = (/5,6,7,8,9,10,11,12,13/), shape = (/3,3/) ), (/ 'f','2' /), (/ 4.0, 5.0 /) ) )
   call foo ( b2 )
   print *, allocated(b2%i), allocated(b2%c), allocated(b2%r)
   print *, b2%i, b2%c, b2%r

   allocate ( b3, source = base(4,1)( reshape ( source = (/1/), shape = (/1,1/) ), (/'x'/), (/1.0/) ) )
   deallocate ( b3%c, b3%r )
   allocate ( b3%c(1:0) )
   allocate ( b3%r(1:0) )

   call foo ( b3 )
   print *, allocated(b3%i), allocated(b3%c), allocated(b3%r)
   print *, b3%i, b3%c, b3%r

end program

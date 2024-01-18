! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn002l
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.9.3: Inquire by output list
!*                               - Try output item function return
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   contains
      procedure, pass :: getBase
   end type

contains
   elemental function getBase(a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      type(base(3)) :: getBase ! tcx: (3)
      select type (a)
         type is (base(*)) ! tcx: (*)
            getBase = a
      end select
   end function
end module

program funcRetrn002l
   use m1

   interface
      elemental function getBase1(a)
         import base
         type(base(3))  :: getBase1 ! tcx: (3)
         class(base(*)), intent(in) :: a ! tcx: (*)
      end function
   end interface

   ! declaration of variables
   class(base(:)), allocatable :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), pointer :: b3(:), b4(:,:)  ! tcx: (:)
   integer :: length1 = 0

   ! allocation of variables

   allocate(b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b2(2,2), source = reshape( source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b3(4), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b4(2,2), source = reshape( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   inquire (iolength = length1)   b1%getBase()
   if ( length1 /= 12 ) error stop 101_4
   length1=0
   inquire (iolength = length1)   b2%getBase()
   if ( length1 /= 12 ) error stop 2_4
   length1=0
   inquire (iolength = length1)  getBase1(b3), b3%getBase()
   if ( length1 /= 24 ) error stop 3_4
   length1=0

   select type ( b14 => b4 )
      type is (base(*)) ! tcx: (*)
         inquire (iolength = length1)  b14%getBase(),getBase1(b14)
         if ( length1 /= 24 ) error stop 4_4
   end select

end program

elemental function getBase1(a)
   use m1
   type(base(3))  :: getBase1 ! tcx: (3)
   class(base(*)), intent(in) :: a ! tcx: (*)

   select type (a)
      type is (base(*)) ! tcx: (*)
         getBase1 = a
   end select
end function

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 27 changes

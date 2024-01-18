! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.9.3: Inquire by output list
!*                               - Inquire inside an elemental procedure
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
         procedure, pass :: checkIOLength
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=8
      integer, kind :: kchild_1
      integer(kchild_1)   :: i = 0
   end type


contains

   elemental integer function checkIOlength(a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      integer :: i1
      select type (a)
         type is (base(*)) ! tcx: (*)
            inquire ( iolength = i1 ) a
         type is (child(*,8)) ! tcx: (*,8)
            inquire ( iolength = i1 ) a
      end select

      checkIOlength = i1

   end function

end module

program elemental001kl
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), pointer :: b3(:), b4(:,:)    ! tcx: (:)
   integer :: length1 = 0

   ! allocation of variables

   allocate(b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b2(2,2), source = reshape( source = (/ child(3,8)('abc',1), child(3,8)('def',2), child(3,8)('ghi',3), child(3,8)('jkl',4) /), shape=(/2,2/) )) ! tcx: (3,8) ! tcx: (3,8) ! tcx: (3,8) ! tcx: (3,8)
   allocate(b3(4), source = (/ child(3,8)('ABC',1), child(3,8)('DEF',2), child(3,8)('GHI',3), child(3,8)('JKL',4) /) ) ! tcx: (3,8) ! tcx: (3,8) ! tcx: (3,8) ! tcx: (3,8)
   allocate(b4(2,2), source = reshape( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   print *,b1%checkIOlength()
   print *,b2%checkIOlength()
   print *,b3%checkIOlength()
   print *,b4%checkIOlength()

end program



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 12 changes
! type: child - added parameters (kchild_1) to invoke with (3,8) / declare with (*,8) - 9 changes

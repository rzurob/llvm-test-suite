! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - inquire iolength of scalar polymorphic items
!*                                 when output items are structure/array constructor
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
   type :: base (kbase_1,kbase_2,lbase_1) ! kbase_1,kbase_2,lbase_1=4,8,3
      integer, kind :: kbase_1,kbase_2
      integer, len :: lbase_1
      integer(kbase_2)   :: x
      real(kbase_1)      :: y
      character(lbase_1) :: z
   end type

   type, extends(base) :: child (kchild_1,kchild_2) ! kchild_1,kchild_2=8,2
      integer, kind :: kchild_1,kchild_2
      integer(kchild_2) :: a
      real(kchild_1)    :: b
      complex(kchild_1) :: c
   end type

   interface
      integer function getIOlengthArray (item)
         class(*) :: item(:)
      end function
   end interface

   interface
      integer function getIOlength (item)
         class(*) :: item
      end function
   end interface
end module

program constructor001klk
   use m1

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   if ( getIOlength ( base(4,8,3)(x=1, y=2.2, z='abc') ) /= 16 )            error stop 101_4  ! tcx: (4,8,3)

   if ( getIOlength ( child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)) ) /= 48 )     error stop 2_4  ! tcx: (4,8,3,8,2)

   if ( getIOlengthArray ( (/ base(4,8,3)(x=1, y=2.2, z='abc'), base(4,8,3)(x=3, y=4.5, z='abc') /) ) /= 32  )             error stop 3_4    ! tcx: (4,8,3) ! tcx: (4,8,3)

   if ( getIOlengthArray ( (/ ( child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1,-2 ) /) )      /= 240 )             error stop 4_4  ! tcx: (4,8,3,8,2)

   if ( getIOlengthArray ( (/ ( child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1 ) /) )         /= 0 )               error stop 5_4  ! tcx: (4,8,3,8,2)


end program

integer function getIOlength (item)
use m1, only: base, child
   class(*) :: item
   integer :: length1 = -999999
   select type ( a => item )
      type is (base(4,8,*)) ! tcx: (4,8,*)
         inquire ( iolength = length1 ) a
      type is (child(4,8,*,8,2)) ! tcx: (4,8,*,8,2)
         inquire ( iolength = length1 ) a
      type is (integer)
         inquire ( iolength = length1 ) a
      class default
         error stop 6_4
   end select

   getIOlength = length1

end function

integer function getIOlengthArray (item)
use m1, only: base, child
   class(*) :: item(:)
   integer :: length1 = -999999
   select type ( a => item )
      type is (base(4,8,*)) ! tcx: (4,8,*)
         inquire ( iolength = length1 ) a
      type is (child(4,8,*,8,2)) ! tcx: (4,8,*,8,2)
         inquire ( iolength = length1 ) a
      type is (integer)
         inquire ( iolength = length1 ) a
      class default
         error stop 7_4
   end select

   getIOlengthArray = length1

end function

! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2,lbase_1) to invoke with (4,8,3) / declare with (4,8,*) - 5 changes
! type: child - added parameters (kchild_1,kchild_2) to invoke with (4,8,3,8,2) / declare with (4,8,*,8,2) - 5 changes

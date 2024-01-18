! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate001klk
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

end module

program associate001klk
   use m1

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   procedure(integer) :: getIOlength

   associate ( a => base(4,8,3)(x=1, y=2.2, z='abc'), b => child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)), c => (/ base(4,8,3)(x=1, y=2.2, z='abc'), base(4,8,3)(x=3, y=4.5, z='abc') /), & ! tcx: (4,8,3) ! tcx: (4,8,3) ! tcx: (4,8,3) ! tcx: (4,8,3,8,2)
               d => (/ ( child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1,-2 ) /), e => (/ ( child(4,8,3,8,2)(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1 ) /) ) ! tcx: (4,8,3,8,2) ! tcx: (4,8,3,8,2)

      inquire ( iolength = length1 ) a
      if ( length1 /= 16 )            error stop 101_4

      inquire ( iolength = length1 ) b
      if ( length1 /= 48 )            error stop 2_4

      inquire ( iolength = length1 ) c
      if ( length1 /= 32 )            error stop 3_4

      inquire ( iolength = length1 ) d
      if ( length1 /= 240 )           error stop 4_4

      inquire ( iolength = length1 ) e
      if ( length1 /= 0 )             error stop 5_4

   end associate

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2,lbase_1) to invoke with (4,8,3) / declare with (4,8,*) - 3 changes
! type: child - added parameters (kchild_1,kchild_2) to invoke with (4,8,3,8,2) / declare with (4,8,*,8,2) - 3 changes

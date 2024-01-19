! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (array variable)
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

   type base (kb) ! kb=4
      integer, kind :: kb
      integer(kb)   :: i = 9
   end type

   type, extends(base), abstract ::  child (kc) ! kc=4
      integer, kind :: kc
      real(kc)      :: r = 9.0
   end type

   type, extends(child) :: gen3 (lg) ! lg=3
      integer, len :: lg
      character(lg) :: c = 'xxx'
   end type

end module

program input105kl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base(4))               :: b1(2) ! tcx: (4)
   type(base(4)), pointer      :: b2(:) ! tcx: (4)
   type(gen3(4,4,3))               :: b3(2,2) ! tcx: (4,4,3)
   type(gen3(4,4,:)), allocatable  :: b4(:,:) ! tcx: (4,4,:)

   namelist /n1/ b1, b2
   namelist /n1/ b3, b4

   allocate(base(4):: b2(3)) ! tcx: base(4)
   allocate(gen3(4,4,3):: b4(3,3)) ! tcx: gen3(4,4,3)

   open (1, file='input105kl.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4

   print *, b1
   print *, b2
   print *, b3
   print *, b4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 0 changes
! type: gen3 - added parameters (lg) to invoke with (4,4,3) / declare with (4,4,*) - 2 changes

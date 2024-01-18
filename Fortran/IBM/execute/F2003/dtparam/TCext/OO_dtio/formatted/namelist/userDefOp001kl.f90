! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 09/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment with DTIO and namelist formatting
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type :: base (kb)
      integer, kind :: kb
      integer(kb) :: i1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   :: i2
   end type

   interface operator(+)
      class(base(4)) function myAdd(a,b) ! tcx: (4)
         import base, child
         class(base(4)), intent(in) :: a, b ! tcx: (4)
         allocatable :: myAdd
      end function
   end interface

   interface assignment(=)
      subroutine myAsgn(a,b)
         import base, child
         class(base(4)), intent(out) :: a ! tcx: (4)
         class(base(4)), intent(in)  :: b ! tcx: (4)
      end subroutine
   end interface

   class(base(4)), allocatable :: b1, b2, b3, b4 ! tcx: (4)
   namelist /n1/ b1, b2

end module

program userDefOp001kl
   use m

   open ( 1, file='userDefOp001kl.1', form='formatted', access='sequential' )

   allocate ( b1, source = child(4,4) ( 1000, 1001 ) ) ! tcx: (4,4)
   allocate ( b2, source = child(4,4) ( 2000, 2002 ) ) ! tcx: (4,4)
   allocate ( child(4,4) :: b3 ) ! tcx: (4,4)

   b3 = b1 + b2

   deallocate ( b1, b2 )

   allocate ( b1, source = base(4) ( 100 ) ) ! tcx: (4)
   allocate ( b2, source = base(4) ( 200 ) ) ! tcx: (4)
   allocate ( base(4) :: b4 ) ! tcx: (4)

   b4 = b1 + b2

end program

class(base(4)) function myAdd(a,b) ! tcx: (4)
   use m, only: base, child, n1

   interface write(formatted)
      subroutine writeFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base(4)), intent(in) :: a, b ! tcx: (4)
   allocatable :: myAdd

   namelist /add/ myAdd

   if ( .not. same_type_as ( a, b ) ) error stop 1_4

   select type ( a )
      type is ( base(4) ) ! tcx: (4)
         allocate ( myAdd, source = base(4) ( i1= ( a%i1+b%i1 ) ) ) ! tcx: (4)
         write ( 1, n1, iostat = stat, iomsg = msg )
         write ( 1, add, iostat = stat, iomsg = msg )
      type is ( child(4,4) ) ! tcx: (4,4)
         select type ( b )
            type is ( child(4,4) ) ! tcx: (4,4)
               allocate ( myAdd, source = child(4,4) ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) ) ! tcx: (4,4)
               write ( 1, n1, iostat = stat, iomsg = msg )
               write ( 1, add, iostat = stat, iomsg = msg )
            class default
               error stop 2_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a ! tcx: (4)
   class(base(4)), intent(in)  :: b ! tcx: (4)

   if ( .not. same_type_as ( a, b ) ) error stop 3_4

   select type ( b )
      type is (base(4)) ! tcx: (4)
        a%i1 = b%i1
      type is (child(4,4)) ! tcx: (4,4)
        select type ( a )
           type is ( child(4,4) ) ! tcx: (4,4)
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 4_4
        end select
   end select

end subroutine

subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   select type ( dtv )
      type is (base(4)) ! tcx: (4)
         write (unit, "('i1=',I4)", iostat=iostat, iomsg=iomsg ) dtv%i1
      type is (child(4,4)) ! tcx: (4,4)
         write (unit, "('i1=',I4,1X,'i2=',I4)", iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 18 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 9 changes

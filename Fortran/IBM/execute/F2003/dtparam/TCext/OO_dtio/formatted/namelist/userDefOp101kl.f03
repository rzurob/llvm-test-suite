! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 09/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing:  User-defined operator and assignment with DTIO
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

end module

program userDefOp101kl
   use m

   class(base(4)), allocatable :: b1, b2, b3, b4 ! tcx: (4)

   allocate ( child(4,4) :: b1, b2 ) ! tcx: (4,4)
   allocate ( child(4,4) :: b3 ) ! tcx: (4,4)

   open ( 1, file='userDefOp101kl.1', form='formatted', access='sequential' )

   b3 = b1 + b2

   select type ( b1 )
      type is ( child(4,4) ) ! tcx: (4,4)
         if ( ( b1%i1 /= 1000 ) .or. ( b1%i2 /= 1001 ) ) error stop 1_4
      class default
         error stop 2_4
   end select

   select type ( b2 )
      type is ( child(4,4) ) ! tcx: (4,4)
         if ( ( b2%i1 /= 2000 ) .or. ( b2%i2 /= 2002 ) ) error stop 3_4
      class default
          error stop 4_4
   end select

   select type ( b3 )
      type is ( child(4,4) ) ! tcx: (4,4)
         if ( ( b3%i1 /= 3000 ) .or. ( b3%i2 /= 3003 ) ) error stop 5_4
      class default
         error stop 6_4
   end select

   deallocate ( b1, b2 )
   allocate ( base(4) :: b1, b2 ) ! tcx: (4)
   allocate ( base(4) :: b4 ) ! tcx: (4)

   b4 = b1 + b2

   select type ( b1 )
      type is ( base(4) ) ! tcx: (4)
         if ( ( b1%i1 /= 100 ) ) error stop 7_4
      class default
         error stop 8_4
   end select

   select type ( b2 )
      type is ( base(4) ) ! tcx: (4)
         if ( ( b2%i1 /= 200 ) ) error stop 9_4
      class default
         error stop 10_4
   end select

   select type ( b4 )
      type is ( base(4) ) ! tcx: (4)
         if ( ( b4%i1 /= 300 ) ) error stop 11_4
      class default
         error stop 12_4
   end select

end program

class(base(4)) function myAdd(a,b) ! tcx: (4)
   use m, only: base, child

   interface read(formatted)
      subroutine readFormatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg
   class(base(4)), intent(inout) :: a, b ! tcx: (4)
   allocatable :: myAdd
   namelist /myadd1/ a, b

   if ( .not. same_type_as ( a, b ) ) error stop 13_4

   select type ( a )
      type is ( base(4) ) ! tcx: (4)
         read ( 1, myadd1, iostat = stat, iomsg = msg )
         allocate ( myAdd, source = base(4) ( i1= ( a%i1+b%i1 ) ) ) ! tcx: (4)
      type is ( child(4,4) ) ! tcx: (4,4)
         select type ( b )
            type is ( child(4,4) ) ! tcx: (4,4)
               read ( 1, myadd1, iostat = stat, iomsg = msg )
               allocate ( myAdd, source = child(4,4) ( i1= ( a%i1+b%i1 ), i2= ( a%i2+b%i2 ) ) ) ! tcx: (4,4)
            class default
               error stop 14_4
         end select
   end select

end function

subroutine myAsgn(a,b)
   use m, only: base, child
   class(base(4)), intent(out) :: a ! tcx: (4)
   class(base(4)), intent(in)  :: b ! tcx: (4)

   if ( .not. same_type_as ( a, b ) ) error stop 15_4

   select type ( b )
      type is (base(4)) ! tcx: (4)
        a%i1 = b%i1
      type is (child(4,4)) ! tcx: (4,4)
        select type ( a )
           type is ( child(4,4) ) ! tcx: (4,4)
              a%i1 = b%i1
              a%i2 = b%i2
           class default
              error stop 16_4
        end select
   end select

end subroutine

subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   select type ( dtv )
      type is (base(4)) ! tcx: (4)
         read (unit, "(I3)", iostat=iostat, iomsg=iomsg )       dtv%i1
      type is (child(4,4)) ! tcx: (4,4)
         read (unit, "(I4,1X,I4)", iostat=iostat, iomsg=iomsg ) dtv%i1, dtv%i2
   end select

   iomsg = 'dtiowrite'

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 20 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 11 changes

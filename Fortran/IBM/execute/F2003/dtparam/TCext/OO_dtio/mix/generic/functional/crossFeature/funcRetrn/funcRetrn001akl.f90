!*  ===================================================================
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Function Return
!*                                    -  function result is a (non-)polymorphic scalar entity with unformatted i/o
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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
         procedure, pass :: returnMe
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      class(base(3)) function returnMe(dtv) ! tcx: (3)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         allocatable :: returnMe

         allocate ( returnMe, source = dtv )

      end function

end module

program funcRetrn001akl
   use m

   integer :: stat
   character(200) :: msg
   character(3) :: cc1, cc2, cc3
   integer :: i1

   type(base(:)), pointer     :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2 ! tcx: (:)
   class(child(:,4)), allocatable, target :: c1 ! tcx: (:,4)

   interface
    function returnMeExt (dtv)
        import base
        type(base(*)), intent(in) :: dtv
        type(base(3)) returnMeExt
    end function
   end interface
!   procedure ( type(base(3)) ) :: returnMeExt ! tcx: (3)

   allocate ( b1, source = base(3) ( 'abc' ) ) ! tcx: (3)
   allocate ( b2, source = base(3) ( 'def' ) ) ! tcx: (3)
   allocate ( c1, source = child(3,4) ( 'ghi', 10001 ) ) ! tcx: (3,4)

   open ( 1, file = 'funcRetrn001akl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )    returnMeExt(b1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )    returnMe(b2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )  c1%returnMe()
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )    cc1
   read ( 1, iostat = stat, iomsg = msg )    cc2
   read ( 1, iostat = stat, iomsg = msg )    cc3, i1
   if ( ( cc1 /= 'abc' ) .or. ( cc2 /= 'def' ) .or. ( cc3 /= 'ghi' ) .or. ( i1 /= 10001 ) )  error stop 4_4


end program

type(base(3)) function returnMeExt(dtv) ! tcx: (3)
  use m, only: base
  type(base(*)), intent(in) :: dtv ! tcx: (*)
  returnMeExt = dtv
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 3 changes

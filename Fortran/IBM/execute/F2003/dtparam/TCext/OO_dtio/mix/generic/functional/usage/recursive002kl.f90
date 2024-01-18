!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive002kl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - Recursive Formatted I/O with Polymorphic linked-list
!*                               adaptation: exposed kind, length
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      class(base(:)), pointer :: next => null() ! tcx: (:)
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: read => readc
   end type

   contains

      recursive subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
            type is ( child(*,4) ) ! tcx: (*,4)
               write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end select

         if ( associated(dtv%next) ) then
            write (unit, "(DT)", iostat=iostat, iomsg=iomsg)      dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) )  error stop 101_4
         end if

         iomsg = 'dtiowrite'

      end subroutine

      recursive subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         if ( associated(dtv%next) ) then
            read (unit, "(DT)", iostat=iostat, iomsg=iomsg)       dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )   error stop 2_4
         end if

         iomsg = 'dtioread'

      end subroutine

      recursive subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg)   dtv%c, dtv%i

         if ( associated(dtv%next) ) then
            read (unit, "(DT)", iostat=iostat, iomsg=iomsg)      dtv%next
            if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) )  error stop 3_4
         end if

         iomsg = 'dtioread'

      end subroutine

end module

program recursive002kl
   use m

   class(base(:)), allocatable, target :: b1 ! tcx: (:)
   class(base(:)), pointer             :: b2 ! tcx: (:)

   class(base(:)), pointer :: dummy ! tcx: (:)

   namelist /n1/ b1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = child(3,4) ( c='ABC',i= 101 ) ) ! tcx: (3,4)
   allocate ( b1%next, source = child(3,4) (c= 'DEF', i=102 ) ) ! tcx: (3,4)
   allocate ( b1%next%next, source = base(3) (c= 'GHI' ) ) ! tcx: (3)
   allocate ( b1%next%next%next, source = base(3) (c= 'JKL' ) ) ! tcx: (3)
   allocate ( b1%next%next%next%next, source = child(3,4) (c= 'MNO', i=103 ) ) ! tcx: (3,4)
   allocate ( b1%next%next%next%next%next, source = child(3,4) (c= 'PQR',i= 104 ) ) ! tcx: (3,4)

   allocate ( b2, source = child(3,4)(c='abc',i=105) ) ! tcx: (3,4)
   b2%next => b1%next

   open ( 1, file='recursive002kl.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4

   write ( 1,*, iostat = stat, iomsg = msg )          b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )   error stop 5_4

   allocate ( b2%next, source = child(3,4)(c='def',i=201 ) ) ! tcx: (3,4)
   allocate ( b2%next%next, source = base(3)(c='ghi') ) ! tcx: (3)
   allocate ( b2%next%next%next, source = child(3,4)(c='jkl',i=202 ) ) ! tcx: (3,4)
   allocate ( b2%next%next%next%next, source = base(3)(c='mno') ) ! tcx: (3)
   allocate ( b2%next%next%next%next%next, source = child(3,4)(c='pqr',i=203 ) ) ! tcx: (3,4)

   write ( 1, "(DT,/,DT)", iostat = stat, iomsg = msg )  b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )     error stop 6_4

   rewind 1

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            dummy%c = 'xxx'
         type is ( child(*,4) ) ! tcx: (*,4)
            dummy%c = 'xxx'
            dummy%i = -999
      end select
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            dummy%c = 'xxx'
         type is ( child(*,4) ) ! tcx: (*,4)
            dummy%c = 'xxx'
            dummy%i = -999
      end select
      dummy => dummy%next
   end do

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 7_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 8_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   read ( 1, "(DT,/,DT)", iostat = stat, iomsg = msg )   b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )    error stop 9_4

   dummy => b1
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   dummy => b2
   do while ( associated (dummy) )
      select type ( dummy )
         type is ( base(*) ) ! tcx: (*)
            print *, dummy%c
         type is ( child(*,4) ) ! tcx: (*,4)
            print *, dummy%c, dummy%i
      end select
      dummy => dummy%next
   end do

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 17 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 16 changes

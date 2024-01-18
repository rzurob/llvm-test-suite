! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive001alk
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try linked list data structure with recursive DTIO with class hierarchy
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      class(base(:)), pointer :: next => null() ! tcx: (:)
      character(lbase_1) :: c
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i
   end type

    interface write(unformatted)
        module procedure writeunformatted
    end interface

contains

   recursive subroutine writeunformatted ( dtv, unit, iostat, iomsg )

      class(base(*)), intent(in) :: dtv ! tcx: (*)
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      select type ( dtv )
         type is (base(*)) ! tcx: (*)
            write (unit, iostat=iostat )   dtv%c
         type is (child(*,4)) ! tcx: (*,4)
            write (unit, iostat=iostat )   dtv%c, dtv%i
      end select

      if ( iostat /= 0 ) error stop 7_4

      if ( associated(dtv%next) ) then
         write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 8_4
      end if

      iomsg = 'dtiowrite'

   end subroutine

end module

program recursive001alk
   use m

   integer :: stat

   character(3) :: c1, c2, c3, c4, c5, c6, c51, c61
   integer(4) :: i2, i4, i6, i61

   character(200) :: msg = ''
   class(base(:)), pointer :: head ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)

   open (1, file = 'recursive001alk.1', form='unformatted', access='sequential' )

   allocate(b1, source = base(3) (c='abc')) ! tcx: (3)
   allocate(b2, source = child(3,4)(c='def', i=101)) ! tcx: (3,4)
   allocate(b3, source = base(3) (c='ghi')) ! tcx: (3)
   allocate(b4, source = child(3,4)(c='jkl', i=102)) ! tcx: (3,4)
   allocate(b5, source = base(3) (c='mno')) ! tcx: (3)
   allocate(b6, source = child(3,4)(c='pqr', i=103)) ! tcx: (3,4)

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   head => b1

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   head => b4

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   head => b5

   write (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, c2, i2, c3
   if ( ( c1 /= 'abc' ) .or. ( c2 /= 'def' ) .or. ( i2 /= 101 ) .or. ( c3 /= 'ghi' ) )   error stop 4_4

   read (1, iostat=stat, iomsg = msg)      c4, i4, c5, c6, i6
   if ( ( c4 /= 'jkl' ) .or. ( i4 /= 102 ) .or. ( c5 /= 'mno' ) .or. ( c6 /= 'pqr' ) .or. ( i6 /= 103 ) )    error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c51, c61, i61
   if ( ( c51 /= 'mno' ) .or. ( c61 /= 'pqr' ) .or. ( i61 /= 103 ) )                      error stop 6_4

   close (1, status = 'delete' )

end program



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 4 changes

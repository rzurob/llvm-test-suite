!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg006akl
!*
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being polymorphic assumed-size array entity with unformatted i/o
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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write
         procedure, pass :: read
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         class(base(*)), intent(in) :: dtv(*) ! tcx: (*)
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         class(base(*)), intent(inout) :: dtv(2,*) ! tcx: (*)
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine


      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

      subroutine mywrite ( dtv )
         class(base(*)), intent(in) :: dtv(*) ! tcx: (*)
         integer :: stat
         character(200) :: msg

         write ( 1, iostat = stat, iomsg = msg ) dtv(1:4)
         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4
         end select

      end subroutine

      subroutine myread ( dtv )
         class(base(*)), intent(inout) :: dtv(2,*) ! tcx: (*)
         integer :: stat
         character(200) :: msg

         read ( 1, iostat = stat, iomsg = msg) dtv(1:2,1:2)
         select type ( dtv )
            type is ( base(*) ) ! tcx: (*)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 4_4
         end select

      end subroutine

end module

program dummyArg006
   use m

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), allocatable :: b2(:,:) ! tcx: (:)

   open ( 1, file='dummyArg006.1', form='unformatted', access='sequential' )

   allocate ( b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape ( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) , shape = (/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   call mywrite ( b1((/1,2,3,4/)) )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   deallocate ( b1, b2 )
   allocate ( b1(3:6), source = (/ child(3,4) ( 'ABC', 101 ), child(3,4) ( 'DEF', 102 ), child(3,4) ( 'GHI', 103 ), child(3,4) ( 'JKL', 104 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( b2(2,2), source = reshape ( source = (/ child(3,4) ( 'abc', 201 ), child(3,4) ( 'def', 202 ), child(3,4) ( 'ghi', 203 ), child(3,4) ( 'jkl', 204 ) /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   call mywrite ( b1(3:6) )
   call mywrite ( b2 )
   call mywriteext ( b2 )

   rewind 1

   deallocate ( b1, b2 )
   allocate (base(3):: b1(4), b2(2,2) ) ! tcx: base(3)

   call myread  ( b1 )
   call myread  ( b2 )

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. ( b1(4)%c /= 'jkl' ) .or. &
        ( b2(1,1)%c /= 'ABC' ) .or. ( b2(2,1)%c /= 'DEF' ) .or. ( b2(1,2)%c /= 'GHI' ) .or. ( b2(2,2)%c /= 'JKL' ) ) error stop 5_4

   call myreadext ( b1 )

   if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(2)%c /= 'DEF' ) .or. ( b1(3)%c /= 'GHI' ) .or. ( b1(4)%c /= 'JKL' ) ) error stop 6_4

   deallocate ( b1, b2 )
   allocate ( child(3,4) :: b1(4), b2(2,2) ) ! tcx: (3,4)

   call myread  ( b1 )
   call myread  ( b2 )

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         select type ( b2 )
            type is ( child(*,4) ) ! tcx: (*,4)
               if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(1)%i /= 101 ) .or. &
                    ( b1(2)%c /= 'DEF' ) .or. ( b1(2)%i /= 102 ) .or. &
                    ( b1(3)%c /= 'GHI' ) .or. ( b1(3)%i /= 103 ) .or. &
                    ( b1(4)%c /= 'JKL' ) .or. ( b1(4)%i /= 104 ) .or. &
                    ( b2(1,1)%c /= 'abc' ) .or. ( b2(1,1)%i /= 201 ) .or. &
                    ( b2(2,1)%c /= 'def' ) .or. ( b2(2,1)%i /= 202 ) .or. &
                    ( b2(1,2)%c /= 'ghi' ) .or. ( b2(1,2)%i /= 203 ) .or. &
                    ( b2(2,2)%c /= 'jkl' ) .or. ( b2(2,2)%i /= 204 ))      error stop 7_4
         end select
   end select

   call myreadext ( b1 )

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 201 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 202 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 203 ) .or. &
              ( b1(4)%c /= 'jkl' ) .or. ( b1(4)%i /= 204 ) ) error stop 8_4
   end select

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base, child
   class(base(*)), intent(in) :: dtv(*) ! tcx: (*)
   integer :: stat
   character(200) :: msg

   write ( 1, iostat = stat, iomsg = msg ) dtv(1:4)

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 9_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 10_4
   end select

end subroutine

subroutine myreadext ( dtv )
   use m, only: base, child
   class(base(*)),  intent(inout) :: dtv(2,*) ! tcx: (*)
   integer :: stat
   character(200) :: msg

   read ( 1,  iostat = stat, iomsg = msg ) dtv(1:2,1:2)

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 11_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 12_4
   end select

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 22 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 18 changes

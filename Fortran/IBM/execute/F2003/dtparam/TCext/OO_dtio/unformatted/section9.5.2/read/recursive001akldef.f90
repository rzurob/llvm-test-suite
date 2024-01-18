! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive001akldef
!*
!*  PROGRAMMER                 : David Forster (derived from recursive001a by Robert Ma)
!*  DATE                       : 2007-09-17 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
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
      character(lbase_1) :: c = 'xxx'
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
   end type

    interface read(unformatted)
        module procedure readunformatted
    end interface

contains

   recursive subroutine readunformatted ( dtv, unit, iostat, iomsg )

      class(base(*)), intent(inout) :: dtv ! tcx: (*)
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      select type ( dtv )
         type is (base(*)) ! tcx: (*)
            read (unit, iostat=iostat )   dtv%c
         type is (child(*,4)) ! tcx: (*,4)
            read (unit, iostat=iostat )   dtv%c, dtv%i
      end select

      if ( iostat /= 0 ) error stop 7_4

      if ( associated(dtv%next) ) then
         read(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioread' ) ) error stop 8_4
      end if

      iomsg = 'dtioread'

   end subroutine

end module

program recursive001akldef
   use m

   integer :: stat

   character(200) :: msg = ''
   class(base(:)), pointer :: head ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)

   open (1, file = 'recursive001akldef.1', form='unformatted', access='sequential' )

   allocate(b1, source = base(3) ()) ! tcx: (3)
   allocate(b2, source = child(3,4)()) ! tcx: (3,4)
   allocate(b3, source = base(3) ()) ! tcx: (3)
   allocate(b4, source = child(3,4)()) ! tcx: (3,4)
   allocate(b5, source = base(3) ()) ! tcx: (3)
   allocate(b6, source = child(3,4)()) ! tcx: (3,4)

   ! first linked list
   b1%next => b2
   b2%next => b3

   ! second linked list
   b4%next => b5
   b5%next => b6

   head => b1

   write ( 1, iostat=stat)   'abc','def',101,'ghi'
   if ( stat /= 0 ) error stop 101_4

   write ( 1, iostat=stat)   'jkl',102,'mno','pqr',103
   if ( stat /= 0 ) error stop 2_4

   write ( 1, iostat=stat)   'stu','vwx',104
   if ( stat /= 0 ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4
   
   select type ( b2 )
      type is (child(*,4)) ! tcx: (*,4)
         if (( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 101 ) .or. ( b3%c /= 'ghi' ) ) error stop 5_4
   end select   
   
   head => b4

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 6_4

   select type ( b4 )
      type is (child(*,4)) ! tcx: (*,4)
         select type ( b6 )
            type is (child(*,4)) ! tcx: (*,4)
               if (( b4%c /= 'jkl' ) .or. ( b4%i /= 102 ) .or. ( b5%c /= 'mno' ) .or. ( b6%i /= 103 ) .or. ( b6%c /= 'pqr' ) ) error stop 7_4
         end select
   end select   
   
   head => b5

   read (1, iostat=stat, iomsg=msg)       head
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 8_4
   
   select type ( b6 )
      type is (child(*,4)) ! tcx: (*,4)
         if ( ( b5%c /= 'stu' ) .or. ( b6%i /= 104 ) .or. ( b6%c /= 'vwx' ) ) error stop 9_4
   end select

   close (1, status = 'delete' )

end program



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 8 changes

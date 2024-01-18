! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : recursive001l
!*
!*  PROGRAMMER                 : David Forster (derived from recursive001 by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2
!*                                        Try linked list data structure with recursive DTIO
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
      type(base(:)), pointer :: next => null() ! tcx: (:)
      character(lbase_1) :: c
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
   
      write (unit, iostat=iostat )   dtv%c
      if ( iostat /= 0  ) error stop 7_4
   
      if ( associated(dtv%next) ) then
         write(unit, iostat= iostat, iomsg = iomsg ) dtv%next
         if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowrite' ) ) error stop 8_4
      end if
   
      iomsg = 'dtiowrite'
   
   end subroutine

end module

program recursive001l
   use m

   integer :: stat
   character(9) :: c1, c2
   character(6) :: c3
   character(200) :: msg = ''
   class(base(:)), pointer :: head ! tcx: (:)
   class(base(:)), allocatable, target :: b1, b2, b3, b4, b5, b6 ! tcx: (:)

   open (1, file = 'recursive001l.1', form='unformatted', access='stream' )

   allocate(b1, source = base(3)(c='abc')) ! tcx: (3)
   allocate(b2, source = base(3)(c='def')) ! tcx: (3)
   allocate(b3, source = base(3)(c='ghi')) ! tcx: (3)
   allocate(b4, source = base(3)(c='jkl')) ! tcx: (3)
   allocate(b5, source = base(3)(c='mno')) ! tcx: (3)
   allocate(b6, source = base(3)(c='pqr')) ! tcx: (3)

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

   read (1, iostat=stat, iomsg = msg)      c1
   if ( ( c1 /= 'abcdefghi' ) )                     error stop 4_4
   
   read (1, iostat=stat, iomsg = msg)      c2
   if ( ( c2 /= 'jklmnopqr' ) )                     error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c3
   if ( ( c3 /= 'mnopqr' ) )                        error stop 6_4

   close (1, status = 'delete' )

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 10 changes

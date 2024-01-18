! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : position002l
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.8: FLUSH statement
!*                               - A FLUSH statement has no effect on the file position
!*                                 Stream Access
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
         procedure, pass :: setC
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char
      a%c = char
   end subroutine
end module


program position002l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   class(base(:)), pointer     :: b3(:) ! tcx: (:)
   integer :: stat1, stat2
   character(200) :: msg1 = ''
   character(4)   :: c1 = ''
   character(4)   :: c2 = ''


   ! allocation of variables

   allocate (base(3)::b1,b2) ! tcx: base(3)
   allocate (base(3)::b3(6)) ! tcx: base(3)

   b1%c = 'ibm'
   b2%c = 'ftn'

   ! I/O operations

   open (unit = 1, file ='position002l.data', form='unformatted', access='stream')

   write (1, iostat=stat1, iomsg = msg1)    'abcde'
   if ( stat1 /= 0 )                                                      error stop 101_4

   FLUSH  1

   write (1, iostat=stat1, iomsg = msg1)    'fghij'
   if ( stat1 /= 0 )                                                      error stop 2_4

   FLUSH  1

   write (1, iostat=stat1, iomsg = msg1)    b1                            !<- inside DTIO, it will flush unit 1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )                    error stop 3_4
   msg1 = ''

   FLUSH  1

   write (1, iostat=stat1, iomsg = msg1)    b2                            !<- inside DTIO, it will flush unit 1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) )                    error stop 4_4
   msg1 = ''

   FLUSH  1

   rewind 1

   read (1, iostat=stat1, iomsg = msg1)             b1                    !<- inside DTIO, it will flush unit 1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                     error stop 5_4
   msg1 = ''

   FLUSH  1

   read (1, iostat=stat1, iomsg = msg1, pos=6)      b2                    !<- inside DTIO, it will flush unit 1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                     error stop 6_4
   msg1 = ''

   FLUSH  1

   read (1, iostat=stat1, iomsg = msg1, pos=11)     c1
   if ( stat1 /= 0  )                                                     error stop 7_4
   msg1 = ''

   FLUSH  1

   read (1, iostat=stat1, iomsg = msg1)             c2
   if ( stat1 /= 0  )                                                     error stop 8_4
   msg1 = ''

   FLUSH 1

   read (1, iostat=stat1, iomsg = msg1, pos = 1)    b3                    !<- inside DTIO, it will flush unit 1
   if ( stat1 /= 0  )                                                     error stop 9_4
   msg1 = ''

   ! check if the values are set correctly

   if ( b1%c /= 'abc' )                                                   error stop 10_4
   if ( b2%c /= 'fgh' )                                                   error stop 11_4
   if ( c1   /= 'ibmZ' )                                                  error stop 12_4
   if ( c2   /= 'ftnZ' )                                                  error stop 13_4
   if ( ( b3(1)%c /= 'abc' ) .or. ( b3(2)%c /= 'def' ) .or. ( b3(3)%c /= 'ghi' ) .or. &
        ( b3(4)%c /= 'jib' ) .or. ( b3(5)%c /= 'mZf' ) .or. ( b3(6)%c /= 'tnZ' ) )  error stop 14_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    Flush (unit, iostat=iostat, iomsg=iomsg)

    if ( iostat /= 0 ) error stop 15_4

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 16_4

    Flush (unit, iostat=iostat, iomsg=iomsg)

    if ( iostat /= 0 ) error stop 17_4

    iomsg = 'dtio read'

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    Flush (unit, iostat=iostat, iomsg=iomsg)

    if ( iostat /= 0 ) error stop 18_4

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    if ( iostat /= 0 ) error stop 19_4

    Flush (unit, iostat=iostat, iomsg=iomsg)

    if ( iostat /= 0 ) error stop 20_4

    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

    if ( iostat /= 0 ) error stop 21_4

    Flush (unit, iostat=iostat, iomsg=iomsg)

    iomsg = 'dtio write'

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes

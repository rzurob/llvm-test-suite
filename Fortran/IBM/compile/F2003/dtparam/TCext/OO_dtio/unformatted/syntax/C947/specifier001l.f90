! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : specifier001l
!*
!*  DATE                       : 2007-09-09 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: C947: no specifier should be specified once than once
!*                               Try 5 specifiers including unit=, recl=, exist=, sequential=, and stream=
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
   end type
end module


program specifier001l
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
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   integer :: il, i2
   character(200) :: msg1
   character(3) :: l1, l2

   ! allocation of variables

   allocate (base(3)::b1,b2) ! tcx: base(3)

   b1%c = 'ibm'
   b2%c = ''

   ! I/O operations

   open ( 1, file = 'specifier001l.1data', access='sequential', form='unformatted' )
   open ( 2, file = 'specifier001l.2data', access='direct'    , form='unformatted', recl=3 )
   open ( 3, file = 'specifier001l.3data', access='stream'    , form='unformatted' )


   INQUIRE ( 1, unit = 1, name = msg1 )                           !<- specifying 2 unit=
   INQUIRE ( 2, recl = i1, recl = i2  )                           !<- specifying 2 recl=
   INQUIRE ( 3, stream = l1, stream = l2 )                        !<- specifying 2 stream=

   write ( 1, iostat =i1, iomsg = msg1 )          b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 101_4
   write ( 2, iostat =i1, iomsg = msg1, rec=1 )   b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 2_4
   write ( 3, iostat =i1, iomsg = msg1, pos=1 )   b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 3_4

   rewind 1

   read ( 1, iostat =i1, iomsg = msg1 )           b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'sequential dtio read' ) )       error stop 4_4
   if ( b2%c /= 'ibm' ) error stop 5_4
   b2%c = ''

   read ( 2, iostat =i1, iomsg = msg1, rec=1 )    b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                  error stop 6_4
   if ( b2%c /= 'ibm' ) error stop 7_4
   b2%c = ''

   read ( 3, iostat =i1, iomsg = msg1, pos=1 )    b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                  error stop 8_4
   if ( b2%c /= 'ibm' ) error stop 9_4
   b2%c = ''

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )
   close ( 3, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: sequential1

    INQUIRE ( unit, sequential=sequential1, sequential=sequential1 )   !<- specify 2 sequential=

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( sequential1 == 'YES' ) then
       iomsg = 'sequential dtio read'
    else
       iomsg = 'dtio read'
    end if

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    logical :: l1

    INQUIRE ( unit, exist = l1, exist = l1 )                     !<- specify 2 exist=

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( l1 )     then
       iomsg = 'dtio write'
    else
       iomsg = 'error'
    end if

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
